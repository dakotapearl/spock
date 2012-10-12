with ENV_Test;
with NetworkManagement; use NetworkManagement;
with NetworkNode; use NetworkNode;
with Commenting; use Commenting;
with Genetics; use Genetics;
with DataDomain; use DataDomain;

package body EXP_Test is

   procedure SingleSeedNode (InputRegister : in InputInterfaceArray_Pointer;
                             OutputRegister : in OutputInterfaceArray_Pointer;
                             NodePool : in NodeArray_Pointer) is
      TempNode : NodeObject_Pointer;
   begin
      TempNode := newNodeObject(3,3); --Test values
      NodePool.all.Insert(getNodeID(TempNode.all), TempNode); --combine this...
      for i in InputRegister'Range loop -- and this somewhere
         InputRegister(i).AddNode(TempNode);
      end loop;
      getConnectionsObject(TempNode).setOutputConnections(OutputRegister);
   end SingleSeedNode;

   procedure DualSeedNodes (InputRegister : in InputInterfaceArray_Pointer;
                             OutputRegister : in OutputInterfaceArray_Pointer;
                             NodePool : in NodeArray_Pointer) is
      TempNode1 : NodeObject_Pointer;
      TempNode2 : NodeObject_Pointer;
   begin
      
      TempNode1 := newNodeObject(3,3); --Test values
      NodePool.all.Insert(getNodeID(TempNode1.all), TempNode1); --combine this...
      for i in InputRegister'Range loop -- and this somewhere
         InputRegister(i).AddNode(TempNode1);
      end loop;
      
      TempNode2 := newNodeObject(3,3);
      NodePool.all.Insert(getNodeID(TempNode2.all), TempNode2);
      getConnectionsObject(TempNode1).addNodeConnection(TempNode2);
      getConnectionsObject(TempNode2).setOutputConnections(OutputRegister);
      
   end DualSeedNodes;
   
   procedure LinkToEnvPackage (InputRegister : in InputInterfaceArray_Pointer;
                               OutputRegister : in OutputInterfaceArray_Pointer;
                               NodePool : in NodeArray_Pointer) is
   begin
      null;
   end LinkToEnvPackage;

   procedure setup is
   begin
      
      TestNetwork := newSMSNetwork(NetworkName => "TestNetwork", 
                                   InputCount => 2, 
                                   OutputCount => 1, 
                                   setupProcedure => DualSeedNodes'Access);
      Env := newSMSNetwork(NetworkName => "Environment", 
                           InputCount => 1, 
                           OutputCount => 2, 
                           setupProcedure => LinkToEnvPackage'Access);
      
      SetNetworkAsEnvironment(Env, ENV_Test.Input'Access, ENV_Test.NumberOfInputs, ENV_Test.Outputs);
      
      ConnectNetworks(TestNetwork, Env, ConnectionQuantity => 1);
      ConnectNetworks(Env, TestNetwork, ConnectionQuantity => 2);
      
      c3("(EXP_Test) Setup complete");
      
   end setup;
   
   procedure start is
   begin
      outputTask := new ENV_Test.generateOutput;
      c3("(EXP_Test) Environment 'outputing task' has started");
   end start;
   
   procedure setupGeneticCode is
      gc : GeneticCode_Pointer;
      tc : TemplateCode_Pointer;
      ThresholdValues : PropertyValuesArray_Pointer;

   begin
      tc := newTemplateCode;
      
      SetChildren(tc.root, 2);
      
      SetName(tc.root.Children(1), "Threshold first decimal");
      SetValue(tc.root.Children(1), DD_Integer(1));
      SetAcceptableValues(tc.root.Children(1), DD_Integer_Range(0, 9));
      
   end setupGeneticCode;
   
end EXP_Test;
