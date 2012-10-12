with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with NetworkNode; use NetworkNode;
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Calendar; use Ada.Calendar;

package body NetworkManagement is
   
   procedure Initialise (InitialiseNetworks : access procedure) is
   begin
      
      if cv /= None then
         Put_Line("");
         Put_Line("Instantiating data...");
      end if;
            
      if cv /= None then
         Put_Line("");
         Put_Line("Initialising networks and establishing connections...");
      end if;
      InitialiseNetworks.all;
      
      if cv /= None then
         Put_Line("");
         Put_Line("Startup procedure complete.");
         Put_Line("");
      end if;
   
   end Initialise;
   

   
   function newSMSNetwork (NetworkName : String;
                           InputCount : Natural; 
                           OutputCount : Natural;
                           setupProcedure : access procedure (InputRegister : in InputInterfaceArray_Pointer;
                                                              OutputRegister : in OutputInterfaceArray_Pointer;
                                                              NodePool : in NodeArray_Pointer) 
                           ) return SMSNetwork_Pointer is
      newNetwork : SMSNetwork_Pointer;
   begin
      newNetwork := new SMSNetwork'(id => SMSNetworkIDIndex,
                                    Name => To_Unbounded_String(NetworkName),
                                    InputRegister => new InputInterfaceArrayType (1 .. InputCount),
                                    OutputRegister => new OutputInterfaceArrayType (1 .. OutputCount),
                                    NodePool => new NodeArray.Map);
      
      SMSNetworkIDIndex := SMSNetworkID'Succ(SMSNetworkIDIndex);
      
      for i in newNetwork.InputRegister'Range loop
         newNetwork.InputRegister(i).Initialise(InputInterfaceIDIndex);
         InputInterfaceIDIndex := InputInterfaceID'Succ(InputInterfaceIDIndex);
      end loop;
      for i in newNetwork.OutputRegister'Range loop
         newNetwork.OutputRegister(i).Initialise(OutputInterfaceIDIndex);
         OutputInterfaceIDIndex := OutputInterfaceID'Succ(OutputInterfaceIDIndex);
      end loop;
      
      setupProcedure.all(newNetwork.InputRegister, newNetwork.OutputRegister, newNetwork.NodePool);
      
      c3("Network created: " & To_String(newNetwork.Name) & "(" & Trim(Integer'Image(Integer(newNetwork.id)), Ada.Strings.Left) & ")");
      
      return newNetwork;
   end NewSMSNetwork;

   function availableInputs(network : in SMSNetwork_Pointer) return Natural is
      inputCount : Natural := 0;
   begin
      for i in network.InputRegister.all'Range loop
         if network.InputRegister.all(i).isLinkedTo = false then
            inputCount := inputCount + 1;
         end if;
      end loop;
      return inputCount;
   end availableInputs;
   
   function availableOutputs(network : in SMSNetwork_Pointer) return Natural is
      outputCount : Natural := 0;
   begin
      for i in network.OutputRegister.all'Range loop
         if network.OutputRegister.all(i).interfaceIsSet = false then
            outputCount := outputCount + 1;
         end if;
      end loop;
      return outputCount;
   end availableOutputs;
   
   function NetworksAreCompatible (TransmittingNetwork : in SMSNetwork_Pointer; 
                                  ReceivingNetwork : in SMSNetwork_Pointer; 
                                  ConnectionQuantity : in Natural) return Boolean is
   result : Boolean := true;
   begin
      
      if availableOutputs(TransmittingNetwork) < ConnectionQuantity then
         result := false;
      elsif availableInputs(ReceivingNetwork) < ConnectionQuantity then
         result := false;
      end if;
      
      return result;
   end NetworksAreCompatible;
   
   procedure ConnectNetworks (TransmittingNetwork : in SMSNetwork_Pointer; 
                              ReceivingNetwork : in SMSNetwork_Pointer; 
                              ConnectionQuantity : in Natural) is
      
      function nextAvailableInput (inputs : InputInterfaceArray_Pointer) return InputInterface_Pointer is
         cursor : Integer;
      begin
         cursor := inputs.all'First;
         loop
            exit when (inputs.all(cursor).isLinkedTo = false) or (cursor > inputs.all'Last);
            cursor := Integer'Succ(cursor);
         end loop;
         if cursor > inputs.all'Last then
            Put_Line("Failed to find available inputs - review algorithm");
         end if;
         return inputs(cursor)'Access;
      end nextAvailableInput;
      
      function nextAvailableOutput (outputs : OutputInterfaceArray_Pointer) return OutputInterface_Pointer is
         cursor : Integer;
      begin
         cursor := outputs.all'First;
         loop
            exit when (outputs.all(cursor).interfaceIsSet = false) or (cursor > outputs.all'Last);
            cursor := Integer'Succ(cursor);
         end loop;
         if cursor > outputs.all'Last then
            Put_Line("Failed to find available outputs - review algorithm");
         end if;
         return outputs(cursor)'Access;
      end nextAvailableOutput;
      
   begin   
      
      if NetworksAreCompatible(TransmittingNetwork, ReceivingNetwork, ConnectionQuantity) then
         for i in 1 .. ConnectionQuantity loop
            nextAvailableOutput(TransmittingNetwork.all.OutputRegister).setInputInterface(nextAvailableInput(ReceivingNetwork.all.InputRegister));
         end loop;
         c3("Connected networks: " & To_String(TransmittingNetwork.Name) & "(" & Trim(Integer'Image(Integer(TransmittingNetwork.id)), Ada.Strings.Left) & ") to " & To_String(ReceivingNetwork.Name) & "(" & Trim(Integer'Image(Integer(ReceivingNetwork.id)), Ada.Strings.Left) & "). Connections:" & Integer'Image(ConnectionQuantity));
      else
         Put_Line("Networks are incompatible");
      end if;
      
   end ConnectNetworks;
   
   procedure SetNetworkAsEnvironment (Network : in SMSNetwork_Pointer;
                                      EnvironmentInputProcedure : in Environment_Input;
                                      NumberOfInputs : out Integer;
                                      EnvironmentOutputs : out OutputInterfaceArray_Pointer) is
   begin
      for i in Network.InputRegister'Range loop
         Network.InputRegister.all(i).setEnvironmentInput(EnvironmentInputProcedure);
      end loop;
      NumberOfInputs := Network.InputRegister.all'Last;
      EnvironmentOutputs := Network.OutputRegister;
      
      c3("Network " & To_String(Network.Name) & "(" & Trim(Integer'Image(Integer(Network.id)), Ada.Strings.Left) & ") set as an environment");
      
   end SetNetworkAsEnvironment;
   
   procedure PrintNetworkSummary (Network : in SMSNetwork_Pointer) is
   
      procedure PrintNode(C: in NodeArray.Cursor) is
      begin
         Put_Line("  Node" & Integer'Image(Integer(getNodeID(NodeArray.Element(C).all))) & " has data" & getNodeData(NodeArray.Element(C)));
      end PrintNode;
      
   begin
      Put_Line("");
      Put_Line("Summary of network: " & To_String(Network.Name));
      
      Put_Line("Nodes:");
      Network.NodePool.Iterate(PrintNode'Access);

      Put_Line("Input interfaces:");
      for i in Network.InputRegister.all'Range loop
         Put_Line("  Interface" & Integer'Image(Integer(Network.InputRegister.all(i).getID)) & " has data" & Network.InputRegister.all(i).getInterfaceData);
      end loop;
      
      Put_Line("Output interfaces:");
      for i in Network.OutputRegister.all'Range loop
         Put_Line("  Interface" & Integer'Image(Integer(Network.OutputRegister.all(i).getID)) & " has data" & Network.OutputRegister.all(i).getInterfaceData);
      end loop;
      
      Put_Line("");
   end PrintNetworkSummary;
   
end NetworkManagement;
