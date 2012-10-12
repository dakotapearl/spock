with DataDomain; use DataDomain;
with NetworkManagement; use NetworkManagement;
with NetworkNode; use NetworkNode;

package Environment is

   type environmentvariablestype is record
      Outputs : OutputInterfaceArray_Pointer;
      NumberOfInputs : Integer;
      procedure Input (InterfaceID : in InputInterfaceID; Data : in Data_Pointer);
      task type generateOutput;
      type generateOutput_Pointer is access all generateOutput;
   end record;

end Environment;
