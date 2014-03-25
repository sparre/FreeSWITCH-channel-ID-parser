with
  Ada.Strings.Fixed;

package body FreeSWITCH_Channel_ID_Parser is
   function Matching (Check, Head : in String) return Boolean;
   function Slice (From, After, To : in String) return String;
   function Tail (From, From_Last : in String) return String;

   function "+" (Item : in String)
                return Ada.Strings.Unbounded.Unbounded_String;
   function "+" (Item : in String)
                return GNAT.Sockets.Port_Type;

   function "+" (Item : in String)
                return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String (Item);
   end "+";

   function "+" (Item : in String)
                return GNAT.Sockets.Port_Type is
   begin
      return GNAT.Sockets.Port_Type'Value (Item);
   end "+";

   function Address (Item : in Instance) return String is
      use Ada.Strings.Unbounded;
   begin
      return To_String (Item.Address);
   end Address;

   function Extension (Item : in Instance) return String is
      use Ada.Strings.Unbounded;
   begin
      return To_String (Item.Extension);
   end Extension;

   function Host (Item : in Instance) return String is
      use Ada.Strings.Unbounded;
   begin
      return To_String (Item.Host);
   end Host;

   function Matching (Check, Head : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Head (Check, Head'Length) = Head;
   end Matching;

   function Mode (Item : in Instance) return Modes is
   begin
      return Item.Mode;
   end Mode;

   function Port (Item : in Instance) return GNAT.Sockets.Port_Type is
   begin
      return Item.Port;
   end Port;

   function Slice (From, After, To : in String) return String is
      use Ada.Strings.Fixed;
      Starting_At : Positive := From'First;
      Ending_At   : Positive := From'Last;
      Current     : String renames From (Starting_At .. Ending_At);
   begin
      if Index (Source => Current, Pattern => After) > 0 then
         Starting_At :=
           Index (Source => Current, Pattern => After) + After'Length;

         if Index (Source => Current, Pattern => To) > 0 then
            Ending_At := Index (Source => Current, Pattern => To) - 1;

            return Current;
         else
            raise Constraint_Error
              with """" & From & """ does not contain """ & To &
                   """ after """ & After & """.";
         end if;
      else
         raise Constraint_Error
           with """" & From & """ does not contain """ & After & """.";
      end if;
   end Slice;

   function Tail (From, From_Last : in String) return String is
      use Ada.Strings.Fixed;
      Position : Natural;
   begin
      Position := Index (Source  => From,
                         Pattern => From_Last,
                         Going   => Ada.Strings.Backward);

      if Position > 0 then
         return From (Position + 1 .. From'Last);
      else
         raise Constraint_Error
           with """" & From & """ does not contain """ & From_Last & """.";
      end if;
   end Tail;

   function Value (Image : in String) return Instance is
      Head_Internal : constant String := "sofia/internal/";
      Head_External : constant String := "sofia/external/";
   begin
      if Matching (Check => Image, Head => Head_Internal) then
         return (Mode      => Internal,
                 Extension => +Slice (Image, Head_Internal & "sip:", "@"),
                 Host      => +Slice (Image, "@", ":"),
                 Port      => +Tail  (Image, From_Last => ":"));
      elsif Matching (Check => Image, Head => Head_External) then
         return (Mode    => External,
                 Address => +Image (Image'First + Head_External'Length ..
                                    Image'Last));
      else
         raise Constraint_Error
           with """" & Image & """ contains an unknown kind of channel ID.";
      end if;
   end Value;
end FreeSWITCH_Channel_ID_Parser;
