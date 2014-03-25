with
  GNAT.Sockets;

private
with
  Ada.Strings.Unbounded;

package FreeSWITCH_Channel_ID_Parser is
   type Instance (<>) is tagged private;
   subtype Class is Instance'Class;

   type Modes is (Internal, External);

   function Value (Image : in String) return Instance;

   function Mode (Item : in Instance) return Modes;

   function Extension (Item : in Instance) return String;
   function Host      (Item : in Instance) return String;
   function Port      (Item : in Instance) return GNAT.Sockets.Port_Type;

   function Address (Item : in Instance) return String;
private
   type Instance (Mode : Modes) is tagged
      record
         case Mode is
            when Internal =>
               Extension : Ada.Strings.Unbounded.Unbounded_String;
               Host      : Ada.Strings.Unbounded.Unbounded_String;
               Port      : GNAT.Sockets.Port_Type;
            when External =>
               Address   : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;
end FreeSWITCH_Channel_ID_Parser;
