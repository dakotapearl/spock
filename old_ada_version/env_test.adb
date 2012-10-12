with Ada.Text_IO; use Ada.Text_IO;
with DataDomain; use DataDomain;
with NetworkManagement; use NetworkManagement;
with Commenting; use Commenting;
with NetworkNode; use NetworkNode;
with Genetics; use Genetics;

package body ENV_Test is
   
   procedure Input (InterfaceID : in InputInterfaceID; Data : in Data_Pointer) is
   begin
      Put_Line("(ENV_Test) Received input to test environment: " & Data_ToString(data.all) & " from input interface" & Integer'Image(Integer(InterfaceID)));
   end input;
   
   task body generateOutput is
      i : Integer := 0;
   begin
      loop
         
         --send
         c1("(ENV_Test) Sending data " & Data_ToString(DataCell(DD_Exponential(i).all)) & "...");
         Outputs(Outputs'First).acceptData(DD_Exponential(i));
         exit when i >= 9;
         i := i + 1;
         
         --wait
         delay 5.0;
         c3("");
         c3("");
         
      end loop;
   end generateOutput;
   
   
end ENV_Test;
