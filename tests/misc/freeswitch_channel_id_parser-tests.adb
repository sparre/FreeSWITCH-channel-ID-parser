with
  FreeSWITCH_Channel_ID_Parser_Tests.Value;

package body FreeSWITCH_Channel_ID_Parser.Tests is
   function Suite return Ahven.Framework.Test_Suite is
      use Ahven.Framework;
      Value_Test : FreeSWITCH_Channel_ID_Parser_Tests.Value.Test;
   begin
      return Suite : Test_Suite :=
                       Create_Suite ("FreeSWITCH_Channel_ID_Parser")
      do
         Add_Static_Test (Suite, Value_Test);
      end return;
   end Suite;
end FreeSWITCH_Channel_ID_Parser.Tests;
