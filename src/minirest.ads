with AAA.Strings;

package Minirest is

   Rest_Error : exception;

   type Status_Kinds is (Informative,   -- 1xx
                         Success,       -- 2xx
                         Redirect,      -- 3xx
                         Client_Error,  -- 4xx
                         Server_Error); -- 5xx

   type Parameter (<>) is private;
   --  A key = value pair

   function "=" (Key, Value : String) return Parameter;

   type Parameters is private;
   --  A collection of arguments

   No_Arguments : constant Parameters;

   function "and" (L : Parameters; R : Parameter) return Parameters;

   function "=" (L, R : String) return Parameters;

   type Response (Status : Status_Kinds; Status_Length : Natural) is record
      Status_Line : String (1 .. Status_Length);
      Status_Code : Positive range 100 .. 599;
      Raw_Headers : AAA.Strings.Vector;
      Headers     : AAA.Strings.Map;
      Content     : AAA.Strings.Vector;
   end record;

   function Get (URL       : String;
                 Arguments : Parameters := No_Arguments)
                 return Response;
   --  Use GET to retrieve URL; may raise Rest_Error for unexpected situations.

private

   type Parameter is new String;

   type Parameters is record
      Data : AAA.Strings.Vector;
   end record;

   No_Arguments : constant Parameters := (Data => AAA.Strings.Empty_Vector);

end Minirest;
