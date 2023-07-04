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

   function "=" (Key : String; Value : String)  return Parameters;
   function "=" (Key : String; Value : Boolean) return Parameters;
   function "=" (Key : String; Value : Integer) return Parameters;

   function Image (These : Parameters) return String;

   type Map is new AAA.Strings.Map with null record;

   overriding
   function Contains (This : Map; Key : String) return Boolean;
   --  Case insensitive

   function Get (This : Map; Key : String) return String;
   --  Case insensitive

   subtype Vector is AAA.Strings.Vector;

   type Response (Status : Status_Kinds; Status_Length : Natural) is tagged
      record
         Status_Line : String (1 .. Status_Length);
         Status_Code : Positive range 100 .. 599;
         Raw_Headers : Vector; -- all lines containing headers
         Headers     : Map;
         Content     : Vector; -- all lines containing the response
      end record;

   procedure Check (This : Response)
     with Post => This.Succeeded
     or else raise Rest_Error with "REST error" & This.Status_Code'Image;
   --  NOOP unless not Succeeded

   function Succeeded (This : Response) return Boolean
   is (This.Status = Success);

   function Get (URL       : String;
                 Arguments : Parameters := No_Arguments; -- these are ?key=val
                 Headers   : Parameters := No_Arguments) -- these are Key: Val
                 return Response;
   --  Use GET to retrieve URL; may raise Rest_Error for unexpected situations.
   --  Headers are passed via -H switch to curl.

   function Post (URL     : String;
                  Data    : String     := ""; -- this can be anything
                  Headers : Parameters := No_Arguments) -- these are Key: Val
                  return Response;
   --  Use POST on URL; data is passed with -d

   type Parameter_Encodings is (JSON); -- Only one supported for now

   function Post (URL      : String;
                  Encoding : Parameter_Encodings := JSON;
                  Data     : Parameters := No_Arguments;
                  Headers  : Parameters := No_Arguments)
                  return Response;
   --  Convert data into JSON before calling Post. This is pretty basic at the
   --  time and won't do any escaping or whatever. Also there are no arrays or
   --  nested maps.

private

   package Maps renames AAA.Strings.Maps;

   type Parameters is record
      Data  : Map;
      Types : Map; -- untyped horror: either string or boolean (lowercase)
   end record
     with Type_Invariant =>
       (for all I in Parameters.Data.Iterate =>
          (for some J in Parameters.Types.Iterate =>
                 Maps.Key (I) = Maps.Key (J)));

   No_Arguments : constant Parameters := (others => <>);

end Minirest;
