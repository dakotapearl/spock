with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_Io; use Ada.Text_Io;
with DataDomain; use DataDomain;
with TimingDomain; use TimingDomain;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Tags; use Ada.Tags;
with Genetics; use Genetics;
with Goals; use Goals;

package body networknode is

   function getNodeID (Node : NodeObject) return NodeID is
   begin
      return Node.ID;
   end getNodeID;

   function newNodeObject (DataCapacity : Integer := 3;
                           ConnectionLimit : Integer := 3) return NodeObject_Pointer is
      newNode : NodeObject_Pointer;
   begin
      newNode := new NodeObject'(id => NewNodeIDIndex,
                                 Data => new DataManagementType,
                                 UnprocessedDataQueue => new Data_FIFO.QueueFIFO_Type,
                                 Connections => new ConnectionManagementType,
                                 FiringMechanism => new FiringMechanismTask,
                                 FiringCondition => new FiringConditionTask,
                                 TargetSelection => new TargetSelectionTask,
                                 TransmissionContent => new TransmissionContentTask, 
                                 DataProcessing => new DataProcessingTask, 
                                 GeneticSequence => new GeneticSequenceManagement, 
                                 EnergyEconomics => new EnergyEconomicsTask, 
                                 LifeCycle => new LifeCycleTask);
      
      if NewNodeIDIndex = NodeID'Last then
         Put_Line("Warning: Adding more nodes than is allowed!");
      end if;
      
      NewNodeIDIndex := NodeID'Succ(NewNodeIDIndex);
      
      newNode.FiringMechanism.Initialise (newNode);
      newNode.FiringCondition.Initialise (newNode);
      newNode.TargetSelection.Initialise (newNode);
      newNode.TransmissionContent.Initialise (newNode);
      newNode.DataProcessing.Initialise (newNode);
      newNode.GeneticSequence.Initialise;
      newNode.EnergyEconomics.Initialise (newNode);
      newNode.LifeCycle.Initialise (newNode);
      
      newNode.Data.Initialise(newNode, DataCapacity);
      newNode.Connections.Initialise(newNode, ConnectionLimit);
      
      c3("Node" & Integer'Image(Integer(newNode.id)) & " created");
      
      return newNode;
   end newNodeObject;
   

   function getDataObject (node : in NodeObject_Pointer) return DataManagement_Pointer is
   begin
      return node.all.Data;
   end getDataObject;

   function getConnectionsObject (node : in NodeObject_Pointer) return ConnectionManagement_Pointer is
   begin
      return node.all.Connections;
   end getConnectionsObject;


   function getNodeData (Node : NodeObject_Pointer) return String is
   begin
      return getDataObject(Node).getDataString;
   end getNodeData;

   protected body DataManagementType is
      
      procedure Initialise (argParentNode : in NodeObject_Pointer; argDataCapacity : in Natural) is
      begin
         if DataCapacity = 0 then
            ParentNode := argParentNode;
            DataCapacity := argDataCapacity;
         end if;
      end Initialise;
      
      entry acceptProcessedInput (dataInput : in Data_Pointer) when Integer(Data.Length) < DataCapacity is
      begin
         Data.Insert(getDataID(dataInput), dataInput);
         c2("(Data Management) Accepted data " & Data_ToString(DataCell(dataInput.all)));
      end acceptProcessedInput;
      
      entry deleteData (id : in DataID) when true is
      begin
         Data.Delete(id);
      end deleteData;
      
      -- Testing functions
      
      function returnFirstElement return Data_Pointer is
      begin
         return Data.First_Element;
      end returnFirstElement;
      
      function returnAnyElement return Data_Pointer is
         c : DataArray.Cursor;
      begin
         c := Data.First;
         for i in 0..15 loop
            c := DataArray.Next(c);
         end loop;
         return DataArray.Element(c);
      end returnAnyElement;
      
      function dataIsEmpty return Boolean is
      begin
         return Data.Is_Empty;
      end dataIsEmpty;
      
      function getDataString return String is
      
         Val : Unbounded_String := To_Unbounded_String("");
         
         procedure PrintData (C: in DataArray.Cursor) is
         begin
            Val := Val & " " & Data_ToString(DataArray.Element(C).all);
         end PrintData;
         
      begin
         
         Data.Iterate(PrintData'Access);
         
         return To_String(Val);
         
      end getDataString;
      
   end DataManagementType;


   procedure AcceptInputExternally (argTarget : Targetable_Pointer; Data : in Data_Pointer) is
      
      procedure send (argt : in Targetable'Class; Data : in Data_Pointer) is
      begin
         if argt'tag = TargetableNode'tag then
            TargetableNode(argt).Node.all.UnprocessedDataQueue.all.Push(Data);
         elsif argt'tag = TargetableOutput'tag then
            TargetableOutput(argt).Output.all.acceptData(Data);
         end if;
      end send;
      
   begin
      send(argTarget.all, Data);
   end AcceptInputExternally;


   -- must be able to add system outputs
   protected body ConnectionManagementType is
      
      procedure Initialise (argParentNode : in NodeObject_Pointer; argConnectionLimit : in Natural) is
      begin
         if ConnectionLimit = 0 then
            ParentNode := argParentNode;
            ConnectionLimit := argConnectionLimit;
         end if;         
      end Initialise;
      
      entry addNodeConnection(Node : NodeObject_Pointer) when Integer(Connections.Length) < ConnectionLimit is
      begin
         Connections.Insert(Node.all.id, Node);
         c2("(Connection Management) Added connection to node" & Integer'Image(Integer(GetNodeID(ParentNode.all))));
      end addNodeConnection;
      
      procedure setOutputConnections (Outputs : OutputInterfaceArray_Pointer) is
      begin
         OutputConnections := Outputs;
         c2("(Connection Management) Output connections set in node" & Integer'Image(Integer(GetNodeID(ParentNode.all))));
      end setOutputConnections;
      
      
      -- Testing functions
      
      function AnyConnection return Targetable_Pointer is
         o : TargetableOutput_Pointer;
         n : TargetableNode_Pointer;
      begin
         -- TODO To make better
         -- if there are outputs then return an output
         if OutputConnections /= null then
            o := new TargetableOutput'(Output => OutputConnections.all(OutputConnections.all'First)'Access);
            return Targetable(o.all)'Access;
         else --otherwise return another node
            n := new TargetableNode'(Node => Connections.First_Element);
            return Targetable(n.all)'Access;
         end if;
      end AnyConnection;
      
   end ConnectionManagementType;
   
   
   task body FiringMechanismTask is
      Ready : Boolean := false;
      ParentNode : NodeObject_Pointer;
      TargetNode : Targetable_Pointer;
      SendingData : Data_Pointer;
      
      procedure print_c1_message (argt : in Targetable'Class) is
      begin
         if argt'tag = TargetableNode'tag then
            c1("node" & Integer'Image(Integer(getNodeID(ParentNode.all))) & " -> node" & Integer'Image(Integer(getNodeID(TargetableNode(argt).Node.all))) & " (" & Data_ToString(SendingData.all) & ")");
         elsif argt'tag = TargetableOutput'tag then
            c1("node" & Integer'Image(Integer(getNodeID(ParentNode.all))) & " -> output" & Integer'Image(Integer(TargetableOutput(argt).Output.all.getID)) & " (" & Data_ToString(SendingData.all) & ")");
         end if;
         
      end print_c1_message;
      
   begin

      accept Initialise (Node : in NodeObject_Pointer) do
         ParentNode := Node;
      end Initialise;
      
      loop
      
         accept Fire do
            c2("(Firing Mechanism) Checking if ready to fire...");
            ParentNode.FiringCondition.ReadyToFire (Ready);
            
            if Ready = true then
               c2("(Firing Mechanism) Selecting target...");
               ParentNode.TargetSelection.SelectTarget(TargetNode);
               c2("(Firing Mechanism) Selecting content...");
               ParentNode.TransmissionContent.SelectContent(SendingData);
               c2("(Firing Mechanism) Sending data to node...");
               print_c1_message(TargetNode.all);
               AcceptInputExternally(TargetNode, SendingData);
               c2("(Firing Mechanism) Deleting local data...");
               getDataObject(ParentNode).deleteData(getDataID(SendingData));
               
               Ready := false;
               
            end if;
            
         end Fire;
      
      end loop;
      
   end FiringMechanismTask;
   
   
   -- Extensions
   
   task body FiringConditionTask is
      ParentNode : NodeObject_Pointer;
   begin

      accept Initialise (Node : in NodeObject_Pointer) do
         ParentNode := Node;
      end Initialise;
      
      loop
      
         accept ReadyToFire (Ready : out Boolean) do
            if getDataObject(ParentNode).dataIsEmpty = false then --Testing
               Ready := true;
               c2("(Firing Condition) Ready to fire");
            else
               Ready := false;
            end if;
         end ReadyToFire;
      
      end loop;
      
   end FiringConditionTask;
   
   
   task body TargetSelectionTask is
      ParentNode : NodeObject_Pointer;
   begin

      accept Initialise (Node : in NodeObject_Pointer) do
         ParentNode := Node;
      end Initialise;
      
      loop
      
         accept SelectTarget (Target : out Targetable_Pointer) do -- TODO
            Target := getConnectionsObject(ParentNode).AnyConnection; --Testing
            c2("(Target Selection) Target selected");
         end SelectTarget;
      
      end loop;
      
   end TargetSelectionTask;
   
   
   task body TransmissionContentTask is
      ParentNode : NodeObject_Pointer;
   begin

      accept Initialise (Node : in NodeObject_Pointer) do
         ParentNode := Node;
      end Initialise;
      
      loop
         
         accept SelectContent (OutData : out Data_Pointer) do
            OutData := getDataObject(ParentNode).all.returnFirstElement; --Testing
            c2("(Transmission Content) Content selected");
         end SelectContent;
      
      end loop;
      
   end TransmissionContentTask;


   task body DataProcessingTask is
      ParentNode : NodeObject_Pointer;
      TempData : Data_Pointer;
   begin

      accept Initialise (Node : in NodeObject_Pointer) do
         ParentNode := Node;
      end Initialise;
      
      loop
         
         ParentNode.all.UnprocessedDataQueue.Pop(TempData);
         
         -- Process data:
         
         --TempData.Int_Data := Integer'Succ(TempData.Int_Data); --Testing
         --if TempData.all = DataCell(DD_Exponential(0).all) then
         --   offset := 0.0;
         --End if;
         --if TempData.all = DataCell(DD_Exponential(1).all) then
         --   offset := 0.2;
         --End if;
         --if TempData.all = DataCell(DD_Exponential(2).all) then
         --   offset := 0.4;
         ---End if;
         --if TempData.all = DataCell(DD_Exponential(3).all) then
         --   offset := 0.6;
         --End if;
         ---if TempData.all = DataCell(DD_Exponential(4).all) then
         --   offset := 0.8;
         --End if;
         
         
         -- Give final data to management
         c2("(Data Processing) Data processed");
         getDataObject(ParentNode).acceptProcessedInput(TempData);
         
         
      end loop;
      
   end DataProcessingTask;

      
   protected body GeneticSequenceManagement is
   
      procedure Initialise is
      begin
         if GeneticsIsInitialised then
            code := newGeneticCode;
         else
            Put_Line("Error: Genetics module not initialised.");
         end if;
      end Initialise;
      
      function Breed (gc : in GeneticCode_Pointer) return GeneticCode_Pointer is
      begin
         if code /= null then
            return splice(code, gc);
         end if;
         return null;
      end Breed;
      
      function Mutate return GeneticCode_Pointer is
      begin
         if code /= null then
            return mutate(code, 0.5);
         end if;
         return null;
      end Mutate;
      
      function getValue (id : in String) return Data_Pointer is
      begin
         if code /= null then
            return Genetics.getProperty(code, id);
         end if;
         return null;
      end getValue;
      
      procedure setValue (value : in Data_Pointer; id : in PropertyID) is
      begin
         if code /= null then
            setProperty(code, To_String(id), value);
         end if;
      end setValue;
      
   end GeneticSequenceManagement;


   task body EnergyEconomicsTask is
      NoeuddParent : NodeObject_Pointer;
      Population : float := 1.0;
      Energie : float := 0.0;
      CroissancedEnergie : float := 0.1; --units/s
      SeuildEnergie : float := 1.0 + 0.1 * Float(getInteger(NoeuddParent.GeneticSequence.getValue("Threshold first decimal"))); --TODO
      
      function ValeurdInformations (data : Data_Pointer) return float is
      begin
         return 0.1;
      end ValeurdInformations;
      
   begin

      accept Initialise (Node : in NodeObject_Pointer) do
         NoeuddParent := Node;
      end Initialise;
      
      loop
         
         if Energie > SeuildEnergie then
            NoeuddParent.FiringMechanism.Fire;
            Energie := 0.0;
         end if;
         
         select
            accept EnergyInputEvent (data_received : in Data_Pointer) do
               Energie := Energie + ValeurdInformations(data_received);
            end EnergyInputEvent;
         or
            delay 1.0; --not delay but wait for 1 second
         end select;
         Energie := Energie + CroissancedEnergie;
         
      end loop;
      
   end EnergyEconomicsTask;


   task body LifeCycleTask is
      ParentNode : NodeObject_Pointer;
   begin

      accept Initialise (Node : in NodeObject_Pointer) do
         ParentNode := Node;
      end Initialise;
      
      --Determine events for breeding, etc...
      
   end LifeCycleTask;
   
   
   
   
   
   protected body InputNetworkInterface is
      
      procedure Initialise (argid : in InputInterfaceID) is
      begin
         id := argid;
         c3("Input interface initialised (" & Trim(Integer'Image(Integer(argid)), Ada.Strings.Left) & ")");
      end Initialise;
      
      procedure addNode (Node : in NodeObject_Pointer) is
      begin
         NodeBroadcastRegister.Insert(getNodeID(Node.all), Node);
         c4("Added node" & Integer'Image(Integer(getNodeID(Node.all))) & " to Input Interface" & Integer'Image(Integer(id)));
      end addNode;
      
      procedure acceptData (Data : in Data_Pointer) is
      
         procedure Broadcast_Data(C: in NodeArray.Cursor) is
            target : TargetableNode_Pointer;
         begin --TODO
            
            c1("input" & Integer'Image(Integer(id)) & " -> node" & Integer'Image(Integer(getNodeID(NodeArray.Element(C).all))) & " (" & Data_ToString(DataCell(Data.all)) & ")");
            
            target := new TargetableNode'(Node => NodeArray.Element(C));
            AcceptInputExternally(Targetable(target.all)'Access, Data);
            
         end Broadcast_Data;
      
      begin
         c4("Accepted data " & Data_ToString(DataCell(Data.all)) & " at Input Interface" & Integer'Image(Integer(id)));
         
         QueueHead := new CommRecordLink'(previousRecord => QueueHead,
                                          TimeStamp => Clock,
                                          Data => Data);
         
         
         --TODO add goal based reward assessment here
         
         
         
         --Either broadcast to nodes or send to environment
         if ENV_Input = null then -- If within a non-environment network
            
            NodeBroadcastRegister.Iterate(Broadcast_Data'Access);
            
         else -- for environment networks
            
            c1("input" & Integer'Image(Integer(id)) & " -> environment (" & Data_ToString(DataCell(Data.all)) & ")");
            
            ENV_Input(id, Data);
            
         end if;
      end acceptData;
      
      function latestData return CommRecordLink_Pointer is
      begin
         return QueueHead;
      end latestData;
      
      function getID return InputInterfaceID is
      begin
         return id;
      end getID;
      
      procedure hasBeenLinkedTo is
      begin
         linkedTo := true;
      end hasBeenLinkedTo;
      
      function isLinkedTo return Boolean is
      begin
         return linkedTo;
      end isLinkedTo;
      
      procedure setEnvironmentInput (argENV_Input : Environment_Input) is
      begin
         ENV_Input := argENV_Input;
         c3("Input Interface" & Integer'Image(Integer(id)) & " has been set to an environment input source");
      end setEnvironmentInput;
      
      function getInterfaceData return String is
         Val : Unbounded_String := To_Unbounded_String("");
         QueueCursor : CommRecordLink_Pointer := QueueHead;
      begin
         loop
            exit when QueueCursor = null;
            Val := Val & " " & Data_ToString(QueueCursor.Data.all);
            QueueCursor := QueueCursor.previousRecord;
         end loop;
         return To_String(Val);
      end getInterfaceData;
      
   end InputNetworkInterface;
   
   protected body OutputNetworkInterface is

      procedure Initialise (argid : in OutputInterfaceID) is
      begin
         id := argid;
         c3("Output interface initialsed (" & Trim(Integer'Image(Integer(id)), Ada.Strings.Left) & ")");
      end Initialise;
      
      procedure setInputInterface (InputInterface : in InputInterface_Pointer) is
      begin
         InterfaceBroadcastRegister := InputInterface;
         InputInterface.all.hasBeenLinkedTo;
         c4("Output Interface" & Integer'Image(Integer(id)) & " has been set to Input Interface" & Integer'Image(Integer(InputInterface.getID)));
      end setInputInterface;
      
      procedure acceptData (Data : in Data_Pointer) is
      begin
         c4("Accepted data " & Data_ToString(DataCell(Data.all)) & " at Output Interface" & Integer'Image(Integer(id)));
         QueueHead := new CommRecordLink'(previousRecord => QueueHead,
                                          TimeStamp => Clock,
                                          Data => Data);
         c4("Stored data " & Data_ToString(DataCell(QueueHead.Data.all)) & " at Output Interface" & Integer'Image(Integer(id)));
         
         c1("output" & Integer'Image(Integer(id)) & " -> input" & Integer'Image(Integer(InterfaceBroadcastRegister.all.getID)) & " (" & Data_ToString(DataCell(QueueHead.Data.all)) & ")");
         
         InterfaceBroadcastRegister.all.acceptData(Data);
         
         if InterfaceBroadcastRegister /= null then
            c4("Input interface set.");
         else
            c4("Input interface not set.");
         end if;
      end acceptData;
      
      function latestData return CommRecordLink_Pointer is
      begin
         return QueueHead;
      end latestData;
      
      function getID return OutputInterfaceID is
      begin
         return id;
      end getID;
      
      function interfaceIsSet return Boolean is
         val : Boolean;
      begin
         if InterfaceBroadcastRegister /= null then
            val := true;
         else
            val := false;
         end if;
         return val;
      end interfaceIsSet;
      
      function getInterfaceData return String is
         Val : Unbounded_String := To_Unbounded_String("");
         QueueCursor : CommRecordLink_Pointer := QueueHead;
      begin
         loop
            exit when QueueCursor = null;
            Val := Val & " " & Data_ToString(QueueCursor.Data.all);
            QueueCursor := QueueCursor.previousRecord;
         end loop;
         return To_String(Val);
      end getInterfaceData;
      
   end OutputNetworkInterface;

   
end NetworkNode;
