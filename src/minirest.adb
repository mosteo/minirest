with AAA.Processes;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with GNAT.OS_Lib;

--------------
-- Minirest --
--------------

package body Minirest is

   package OS renames GNAT.OS_Lib;

   -----------
   -- Check --
   -----------

   procedure Check (This : Response) is
   begin
      if This.Succeeded then
         raise Rest_Error with "REST error" & This.Status_Code'Image;
      end if;
   end Check;

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
   -- Contains --
   --------------

   overriding
   function Contains (This : Map; Key : String) return Boolean
   is
      use AAA.Strings;
   begin
      return (for some I in This.Iterate =>
                To_Lower_Case (Maps.Key (I)) = To_Lower_Case (Key));
   end Contains;

   ---------
   -- Get --
   ---------

   function Get (This : Map; Key : String) return String is
      use AAA.Strings;
   begin
      for I in This.Iterate loop
         if To_Lower_Case (Maps.Key (I)) = To_Lower_Case (Key) then
            return This (I);
         end if;
      end loop;

      raise Constraint_Error with "key not found: " & Key;
   end Get;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (Char : Character) return String is
      Hex : String (1 .. 6);
   begin
      Ada.Integer_Text_IO.Put (Hex, Character'Pos (Char), Base => 16);
      return Hex (4 .. 5);
   end To_Hex;

   --------------
   -- Encoding --
   --------------

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

   -----------
   -- "and" --
   -----------

   function "and" (L : Parameters; R : Parameters) return Parameters is
   begin
      return Result : Parameters := L do
         for I in R.Data.Iterate loop
            Result.Data.Insert (AAA.Strings.Maps.Key (I), R.Data (I));
         end loop;
      end return;
   end "and";

   ---------
   -- "=" --
   ---------

   function "=" (Key, Value : String) return Parameters is
   begin
      return P : Parameters do
         P.Data.Insert  (Key, Value);
         P.Types.Insert (Key, "string");
      end return;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Key : String; Value : Boolean) return Parameters is
      use AAA.Strings;
   begin
      return P : Parameters do
         P.Data.Insert  (Key, To_Lower_Case (Value'Image));
         P.Types.Insert (Key, "boolean");
      end return;
   end "=";

   -----------------
   -- Common_Args --
   -----------------

   function Common_Args return Vector
   is (AAA.Strings
       .To_Vector ("curl")
       .Append ("-s")
       .Append ("-i"));

   --------------------
   -- To_Header_Args --
   --------------------

   function To_Header_Args (Headers : Parameters) return Vector
   is
      Result : Vector;
   begin
      for I in Headers.Data.Iterate loop
         Result.Append ("-H");
         Result.Append (AAA.Strings.Maps.Key (I) & ": " & Headers.Data (I));
      end loop;

      return Result;
   end To_Header_Args;

   -------------
   -- To_JSON --
   -------------

   function To_JSON (Data : Parameters) return String
   is

      function Q (S : String) return String is ("""" & S & """");

      use AAA.Strings.Maps;
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := To_Unbounded_String ("{");
   begin
      for I in Data.Data.Iterate loop
         Append (Result,
                 Q (Key (I)) & ":"
                 & (if not Data.Types.Contains (Key (I))
                     or else Data.Types (Key (I)) = "string"
                   then Q (Data.Data (I))
                   else Data.Data (I)));

         if I /= Data.Data.Last then
            Append (Result, ",");
         end if;
      end loop;

      Append (Result, "}");
      return To_String (Result);
   end To_JSON;

   -----------
   -- Image --
   -----------

   function Image (These : Parameters) return String
   is
   begin
      return To_JSON (These);
   end Image;

   ----------------------------
   -- Identify_Response_Code --
   ----------------------------

   function Identify_Response_Code (Status_Line : String) return Integer is
      Code : Integer := -1;
   begin
      for I in Status_Line'Range loop
         if Status_Line (I) = ' ' then
            Code := Integer'Value (Status_Line (I + 1 .. I + 4));
            exit;
         end if;
      end loop;

      if Code = -1 then
         raise Rest_Error with "Malformed status line: " & Status_Line;
      end if;

      return Code;
   end Identify_Response_Code;

   -----------------
   -- To_Response --
   -----------------

   function To_Response (Status_Line : String;
                         Code        : Integer;
                         Raw         : AAA.Processes.Result)
                         return Response
   is
      In_Headers  : Boolean := True;
      Skip        : Boolean := False;
   begin
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
   end To_Response;

   ---------
   -- Get --
   ---------

   Curl : constant OS.String_Access := OS.Locate_Exec_On_Path ("curl");

   function Get (URL       : String;
                 Arguments : Parameters := No_Arguments;
                 Headers   : Parameters := No_Arguments)
                    return Response
   is

      function To_URL_Args (Map : Minirest.Map) return String is
         use AAA.Strings.Maps;
         Flat : AAA.Strings.Vector;
      begin
         for I in Map.Iterate loop
            Flat.Append (Encode (Key (I)) & "=" & Encode (Map (I)));
         end loop;

         return Flat.Flatten ('&');
      end To_URL_Args;

      Curl_Args : Vector := Common_Args;
   begin
      if Curl in null then
         raise Rest_Error with "Could not find 'curl' tool in path";
      end if;

      --  Add request headers

      Curl_Args := Curl_Args.Append (To_Header_Args (Headers));

      declare
         Raw : constant AAA.Processes.Result :=
              AAA.Processes.Run
                (Curl_Args
                 .Append
                   (URL
                    & (if Arguments.Data.Is_Empty
                      then ""
                      elsif (for some C of URL => C = '?')
                      then "&"
                      else "?")
                    & To_URL_Args (Arguments.Data)),
                 Raise_On_Error => False);
      begin
         if Raw.Exit_Code /= 0 then
            raise Rest_Error with
              "curl exited with non-zero error code:" & Raw.Exit_Code'Image;
         end if;

         declare
            Status_Line : constant String := Raw.Output.First_Element;
            Code        : constant Integer :=
                            Identify_Response_Code (Status_Line);
         begin
            return To_Response (Status_Line, Code, Raw);
         end;
      end;
   end Get;

   ----------
   -- Post --
   ----------

   function Post (URL     : String;
                  Data    : String     := ""; -- this can be anything
                  Headers : Parameters := No_Arguments) -- these are Key: Val
                  return Response
   is
      use all type Vector;
      Curl_Args : Vector := Common_Args & "-X" & "POST";
   begin
      if Curl in null then
         raise Rest_Error with "Could not find 'curl' tool in path";
      end if;

      --  Add request headers

      Curl_Args := Curl_Args.Append (To_Header_Args (Headers));

      declare
         Raw : constant AAA.Processes.Result :=
                 AAA.Processes.Run
                   (Curl_Args
                    .Append (URL)
                    .Append
                      (if Data /= ""
                       then To_Vector ("-d") & Data
                       else AAA.Strings.Empty_Vector),
                    Raise_On_Error => False);
      begin
         if Raw.Exit_Code /= 0 then
            raise Rest_Error with
              "curl exited with non-zero error code:" & Raw.Exit_Code'Image;
         end if;

         declare
            Status_Line : constant String := Raw.Output.First_Element;
            Code        : constant Integer :=
                            Identify_Response_Code (Status_Line);
         begin
            return To_Response (Status_Line, Code, Raw);
         end;
      end;
   end Post;

   ----------
   -- Post --
   ----------

   function Post (URL      : String;
                  Encoding : Parameter_Encodings := JSON;
                  Data     : Parameters := No_Arguments;
                  Headers  : Parameters := No_Arguments)
                  return Response
   is
   begin
      return Post (URL,
                   (case Encoding is
                       when JSON => To_JSON (Data)),
                   Headers => Headers);
   end Post;

end Minirest;
