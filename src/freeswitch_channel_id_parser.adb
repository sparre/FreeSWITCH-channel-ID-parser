package body FreeSWITCH_Channel_ID_Parser is
   function Value (Image : in String) return Instance is
   begin
      return (None => Boolean'Value (Image));
   end Value;
end FreeSWITCH_Channel_ID_Parser;
