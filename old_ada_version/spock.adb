with Ada.Text_IO; use Ada.Text_IO;
with NetworkNode; use NetworkNode;
with NetworkManagement; use NetworkManagement;
with DataDomain; use DataDomain;
with EXP_Test;
with Commenting; use Commenting;

-- http://stevehanov.ca/blog/index.php?id=132

procedure Spock is
begin
   
   -- Enable all comments
   SetCommentVerbosity(Additional);
   c1_comments := true; -- Message passes
   c2_comments := false; -- Network node comments
   c3_comments := false; -- Initialisations
   c4_comments := false; -- Network Management interfaces
   
   -- Network Management initialisation with test experiment
   Initialise(EXP_Test.setup'Access);
   
   Put_Line("");
   Put_Line("Starting experiment in 2 seconds...");
   PrintNetworkSummary(EXP_Test.TestNetwork);
   PrintNetworkSummary(EXP_Test.Env);
   delay 2.0; -- wait for user input here
   EXP_Test.start;
   --loop
   --   delay 1.0;
   --   PrintNetworkSummary(EXP_Test.TestNetwork);
   --   PrintNetworkSummary(EXP_Test.Env);
   --end loop;
end Spock;
