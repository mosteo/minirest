with AAA.Strings;

package Minirest is

   Rest_Error : exception;

   type Status_Kinds is (Informative,   -- 1xx
                         Success,       -- 2xx
                         Redirect,      -- 3xx
                         Client_Error,  -- 4xx
                         Server_Error); -- 5xx

   type Parameters is private;
   --  A collection of arguments of type key + value

   No_Arguments : constant Parameters;

   function "and" (L : Parameters; R : Parameters) return Parameters;

   function "=" (Key, Value : String) return Parameters;

   type Response (Status : Status_Kinds; Status_Length : Natural) is tagged
      record
         Status_Line : String (1 .. Status_Length);
         Status_Code : Positive range 100 .. 599;
         Raw_Headers : AAA.Strings.Vector;
         Headers     : AAA.Strings.Map;
         Content     : AAA.Strings.Vector;
      end record;

   function Succeeded (This : Response) return Boolean
   is (This.Status = Success);

   function Get (URL       : String;
                 Arguments : Parameters := No_Arguments; -- these are ?key=val
                 Headers   : Parameters := No_Arguments) -- these are Key: Val
                 return Response;
   --  Use GET to retrieve URL; may raise Rest_Error for unexpected situations.
   --  Headers are passed via -H switch to curl.

private

   type Parameters is record
      Data : AAA.Strings.Map;
   end record;

   No_Arguments : constant Parameters := (Data => AAA.Strings.Empty_Map);

end Minirest;
