package FreeSWITCH_Channel_ID_Parser is
   type Instance is tagged private;
   subtype Class is Instance'Class;

   function Value (Image : in String) return Instance;
private
   type Instance is tagged
      record
         None : Boolean;
      end record;
end FreeSWITCH_Channel_ID_Parser;
