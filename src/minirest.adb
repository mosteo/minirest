with AAA.Processes;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with GNAT.OS_Lib;

package body Minirest is

   package OS renames GNAT.OS_Lib;

   ------------------
   -- Code_To_Kind --
   ------------------

   function Code_To_Kind (Code : Integer) return Status_Kinds
   is (case Code is
          when 100 .. 199 => Informative,
          when 200 .. 299 => Success,
          when 300 .. 399 => Redirect,
          when 400 .. 499 => Client_Error,
          when 500 .. 599 => Server_Error,
          when others     => raise Constraint_Error);

   --------------
   -- Encoding --
   --------------

   function To_Hex (Char : Character) return String is
      Hex : String (1 .. 6);
   begin
      Ada.Integer_Text_IO.Put (Hex, Character'Pos (Char), Base => 16);
      return Hex (4 .. 5);
   end To_Hex;

   function Encoding (Char : Character) return String
   is (case Char is
          when '!' | '#' | '$' | '%' | '&' | ''' | '(' | ')' | '*' | '+' |
               ',' | '/' | ':' | ';' | '=' | '?' | '@' | '[' | ']' | ' '
               => "%" & To_Hex (Char),
          when others => (1 => Char));

   ------------
   -- Encode --
   ------------

   function Encode (S : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Char of S loop
         Append (Result, Encoding (Char));
      end loop;

      return To_String (Result);
   end Encode;

   ---------
   -- "=" --
   ---------

   function "=" (Key, Value : String) return Parameter is
   begin
      return Parameter (Encode (Key) & "=" & Encode (Value));
   end "=";

   -----------
   -- "and" --
   -----------

   function "and" (L : Parameters; R : Parameter) return Parameters is
   begin
      return Result : Parameters := L do
         Result.Data.Append (String (R));
      end return;
   end "and";

   ---------
   -- "=" --
   ---------

   function "=" (L, R : String) return Parameters is
   begin
      return (Data => AAA.Strings.To_Vector (String (Parameter'(L = R))));
   end "=";

   ---------
   -- Get --
   ---------

   Curl : constant OS.String_Access := OS.Locate_Exec_On_Path ("curl");

   function Get (URL       : String;
                 Arguments : Parameters := No_Arguments)
                    return Response
   is
   begin
      if Curl in null then
         raise Rest_Error with "Could not find 'curl' tool in path";
      end if;

      declare
         Raw : constant AAA.Processes.Result :=
              AAA.Processes.Run
                (AAA.Strings
                 .To_Vector ("curl")
                 .Append ("-v")
                 .Append ("-i")
                 .Append (URL
                          & (if (for some C of URL => C = '?')
                             then '&'
                             else '?')
                          & Arguments.Data.Flatten ("&")),
                 Raise_On_Error => False);
      begin
         if Raw.Exit_Code /= 0 then
            raise Rest_Error with
              "curl exited with non-zero error code:" & Raw.Exit_Code'Image;
         end if;

         declare
            Status_Line : constant String := Raw.Output.First_Element;
            Code        : Integer := -1;
            In_Headers  : Boolean := True;
            Skip        : Boolean := False;
         begin

            --  Identify code

            for I in Status_Line'Range loop
               if Status_Line (I) = ' ' then
                  Code := Integer'Value (Status_Line (I + 1 .. I + 4));
                  exit;
               end if;
            end loop;

            if Code = -1 then
               raise Rest_Error with "Malformed status line: " & Status_Line;
            end if;

            --  Fill response

            return R : Response (Code_To_Kind (Code), Status_Line'Length) do
               R.Status_Line := Status_Line;
               R.Status_Code := Code;

               for I in Raw.Output.First_Index + 1 ..
                 Raw.Output.Last_Index
               loop
                  declare
                     Line : constant String := Raw.Output (I);
                  begin
                     if In_Headers and then Line = "" then
                        In_Headers := False;
                        Skip       := True;
                     end if;

                     if In_Headers then
                        R.Raw_Headers.Append (Line);
                        R.Headers.Insert (AAA.Strings.Head (Line, ':'),
                                          AAA.Strings.Trim
                                            (AAA.Strings.Tail (Line, ':')));
                     elsif Skip then
                        Skip := False;
                     else
                        R.Content.Append (Line);
                     end if;
                  end;
               end loop;
            end return;
         end;
      end;
   end Get;

end Minirest;
