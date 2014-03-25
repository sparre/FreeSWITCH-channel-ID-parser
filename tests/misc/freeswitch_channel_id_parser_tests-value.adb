with
  Ada.Exceptions,
  Ada.Text_IO;
with
  GNAT.Sockets;
with
  Ahven;
with
  FreeSWITCH_Channel_ID_Parser;

package body FreeSWITCH_Channel_ID_Parser_Tests.Value is
   use FreeSWITCH_Channel_ID_Parser;

   generic
      Source    : String;
      Fails     : Boolean := False;
      Mode      : Modes := Internal;
      Extension : String := "";
      Host      : String := "";
      Port      : GNAT.Sockets.Port_Type := 0;
      Address   : String := "";
   procedure Parse_And_Check;

   procedure Parse_And_Check is
      use Ahven;
   begin
      declare
         use type GNAT.Sockets.Port_Type;
         Object : constant Instance :=
                    FreeSWITCH_Channel_ID_Parser.Value (Source);
      begin
         if Fails then
            Fail ("Object creation should have failed.");
         else
            case Object.Mode is
               when Internal =>
                  Assert
                    (Condition => Extension = Object.Extension,
                     Message   => "Extension check: " & Extension & " /= " &
                                  Object.Extension);
                  Assert
                    (Condition => Host = Object.Host,
                     Message   => "Host check: " & Host & " /= " &
                                  Object.Host);
                  Assert
                    (Condition => Port = Object.Port,
                     Message   => "Port check: " &
                                  GNAT.Sockets.Port_Type'Image (Port) &
                                  " /= " &
                                  GNAT.Sockets.Port_Type'Image (Object.Port));
               when External =>
                  Assert (Address = Object.Address, "Address check");
            end case;
         end if;
      exception
         when Assertion_Error =>
            raise;
         when E : others =>
            Fail ("Shouldn't raise exception after object is created.  " &
                    Ada.Exceptions.Exception_Name (E) &
                    " was raised with the message """ &
                    Ada.Exceptions.Exception_Message (E) & """.");
      end;
   exception
      when E : Constraint_Error =>
         if Fails then
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Current_Error,
               Item => "Got a constraint error with message """ &
                       Ada.Exceptions.Exception_Message (E));
         else
            raise;
         end if;
   end Parse_And_Check;

   procedure Example_1 is new Parse_And_Check
     (Source => "sofia/external",
      Fails  => True);

   procedure Example_2 is new Parse_And_Check
     (Source => "sofia/internal",
      Fails  => True);

   procedure Example_3 is new Parse_And_Check
     (Source    => "sofia/internal/sip:1002@192.168.1.178:5760",
      Mode      => Internal,
      Extension => "1002",
      Host      => "192.168.1.178",
      Port      => 5760);

   procedure Example_4 is new Parse_And_Check
     (Source    => "sofia/external/7000@192.168.1.178:7000",
      Mode      => External,
      Address   => "7000@192.168.1.178:7000");

   pragma Style_Checks (Off);
   overriding
   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      T.Set_Name ("Parsing validation");

      Add_Test_Routine (T, Example_1'Access, "1: Parse failure");
      Add_Test_Routine (T, Example_2'Access, "2: Parse failure");
      Add_Test_Routine (T, Example_3'Access, "3: Internal");
      Add_Test_Routine (T, Example_4'Access, "4: External");
   end Initialize;
end FreeSWITCH_Channel_ID_Parser_Tests.Value;
