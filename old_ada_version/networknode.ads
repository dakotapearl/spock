with DataDomain; use DataDomain;
with TimingDomain; use TimingDomain;
with Ada.Containers.Ordered_Maps;
with Constants; use Constants;
with Queue_FIFO;
with Commenting; use Commenting;
with Ada.Calendar; use Ada.Calendar;
with Genetics; use Genetics;

package NetworkNode is
   
   type NodeID is range 0 .. NodeID_Limit;   
   type NodeObject is limited private;
   type NodeObject_Pointer is access all NodeObject;
   function getNodeID (Node : NodeObject) return NodeID;
   
   type Targetable is tagged null record;
   type Targetable_Pointer is access all Targetable;
   type TargetableNode is new Targetable with record
      Node : NodeObject_Pointer;
   end record;
   type TargetableNode_Pointer is access all TargetableNode;
   
   function getNodeData (Node : NodeObject_Pointer) return String;
   
   package Data_FIFO is new Queue_FIFO(Data_Pointer);
   type Data_FIFO_Pointer is access all Data_FIFO.QueueFIFO_Type;
   
   -- NodeObject functions
   --function getNodeID (node : in NodeObject_Pointer) return NodeID;
   function newNodeObject (DataCapacity : Integer := 3;
                           ConnectionLimit : Integer := 3) return NodeObject_Pointer;
   
   -- Responsible for managing inputs and holding important data.
   package DataArray is new Ada.Containers.Ordered_Maps (Element_Type => Data_Pointer,
                                                         Key_Type => DataID);
                                                         
   protected type DataManagementType is
      procedure Initialise (argParentNode : in NodeObject_Pointer; argDataCapacity : in Natural);
      entry acceptProcessedInput (dataInput : in Data_Pointer);
      entry deleteData (id : in DataID);
      
      --Testing functions
      function returnFirstElement return Data_Pointer;
      function returnAnyElement return Data_Pointer;
      function dataIsEmpty return Boolean;
      function getDataString return String;
      
   private   
      Data : DataArray.Map;
      ParentNode : NodeObject_Pointer;
      DataCapacity : Natural := 0;
   end DataManagementType;
   type DataManagement_Pointer is access all DataManagementType;
   function getDataObject (node : in NodeObject_Pointer) return DataManagement_Pointer;
   
   procedure AcceptInputExternally (argTarget : Targetable_Pointer; Data : in Data_Pointer);
   
   
   task type FiringMechanismTask is
      entry Initialise (Node : in NodeObject_Pointer);
      entry Fire;
   end FiringMechanismTask;
   type FiringMechanism_Pointer is access all FiringMechanismTask;
   
   -- Extensions
   
   task type FiringConditionTask is
      entry Initialise (Node : in NodeObject_Pointer);
      entry ReadyToFire (Ready : out Boolean);
   end FiringConditionTask;
   type FiringCondition_Pointer is access all FiringConditionTask;
   
   
   task type TargetSelectionTask is
      entry Initialise (Node : in NodeObject_Pointer);
      entry SelectTarget (Target : out Targetable_Pointer);
   end TargetSelectionTask;
   type TargetSelection_Pointer is access all TargetSelectionTask;
   

   task type TransmissionContentTask is
      entry Initialise (Node : in NodeObject_Pointer);
      entry SelectContent (OutData : out Data_Pointer);
   end TransmissionContentTask;
   type TransmissionContent_Pointer is access all TransmissionContentTask;
   
   
   task type DataProcessingTask is
      entry Initialise (Node : in NodeObject_Pointer);
   end DataProcessingTask;
   type DataProcessing_Pointer is access all DataProcessingTask;

   protected type GeneticSequenceManagement is
      procedure Initialise;
      function Breed (gc : in GeneticCode_Pointer) return GeneticCode_Pointer;
      function Mutate return GeneticCode_Pointer;
      function getValue (id : in String) return Data_Pointer;
      procedure setValue (value : in Data_Pointer; id : in PropertyID);
   private
      code : GeneticCode_Pointer := null;
   end GeneticSequenceManagement;
   type GeneticSequence_Pointer is access all GeneticSequenceManagement;
   
   task type EnergyEconomicsTask is
      entry Initialise (Node : in NodeObject_Pointer);
      entry EnergyInputEvent (data_received : in Data_Pointer);
   end EnergyEconomicsTask;
   type EnergyEconomics_Pointer is access all EnergyEconomicsTask;
   

   task type LifeCycleTask is
      entry Initialise (Node : in NodeObject_Pointer);
   end LifeCycleTask;
   type LifeCycle_Pointer is access all LifeCycleTask;
   
   
   
   type InputInterfaceID is range 1 .. InputInterfaceID_Limit;
   type OutputInterfaceID is range 1 .. OutputInterfaceID_Limit;

   type CommRecordLink;
   type CommRecordLink_Pointer is access all CommRecordLink;
   type CommRecordLink is record
      previousRecord : CommRecordLink_Pointer;
      TimeStamp : Time;
      Data : Data_Pointer;
   end record;
   
   package NodeArray is new Ada.Containers.Ordered_Maps (Element_Type => NodeObject_Pointer,
                                                         Key_Type => NodeID);
   type NodeArray_Pointer is access all NodeArray.Map;

   type Environment_Input is access procedure (InterfaceID : in InputInterfaceID; data : in Data_Pointer);      

   -- this implementation of keeps only the most recent data.
   protected type InputNetworkInterface is
      procedure Initialise (argid : in InputInterfaceID);
      procedure AddNode (Node : in NodeObject_Pointer);
      procedure acceptData (Data : in Data_Pointer);
      function latestData return CommRecordLink_Pointer;
      function getID return InputInterfaceID;
      procedure hasBeenLinkedTo;
      function isLinkedTo return Boolean;
      procedure setEnvironmentInput (argENV_Input : Environment_Input);
      
      function getInterfaceData return String;
      
   private
      linkedTo : Boolean := false;
      id : InputInterfaceID;
      QueueHead : CommRecordLink_Pointer;
      NodeBroadcastRegister : NodeArray.Map;
      ENV_Input : Environment_Input := null;
   end InputNetworkInterface;
   type InputInterface_Pointer is access all InputNetworkInterface;
   type InputInterfaceArrayType is array (Integer range <>) of aliased InputNetworkInterface;
   type InputInterfaceArray_Pointer is access all InputInterfaceArrayType;   

   
   -- this implementation allows an output interface to send data to only one input interface
   --type BroadcastRegisterType is array (Integer range <>) of InputInterfaceArray_Pointer;
   --type BroadcastRegister_Pointer is access all BroadcastRegisterType;   
   -- this implementation of keeps only the most recent data.
   protected type OutputNetworkInterface is
      procedure Initialise (argid : in OutputInterfaceID);
      procedure setInputInterface (InputInterface : in InputInterface_Pointer);
      procedure acceptData (Data : in Data_Pointer);
      function latestData return CommRecordLink_Pointer;
      function getID return OutputInterfaceID;
      function interfaceIsSet return Boolean;
      
      function getInterfaceData return String;
      
   private
      id : OutputInterfaceID;
      QueueHead : CommRecordLink_Pointer;
      InterfaceBroadcastRegister : InputInterface_Pointer := null;
   end OutputNetworkInterface;
   type OutputInterface_Pointer is access all OutputNetworkInterface;
   type OutputInterfaceArrayType is array (Integer range <>) of aliased OutputNetworkInterface;
   type OutputInterfaceArray_Pointer is access all OutputInterfaceArrayType;


   type TargetableOutput is new Targetable with record
      Output : OutputInterface_Pointer;
   end record;
   type TargetableOutput_Pointer is access all TargetableOutput;


   -- Responsible for managing connections with other node objects
   package ConnectionArray is new Ada.Containers.Ordered_Maps (Element_Type => NodeObject_Pointer,
                                                               Key_Type => NodeID);
   --type ConnectionArray is array (Integer range 1 .. SynapseCapacity) of NodeObject_Pointer;
   protected type ConnectionManagementType is
      procedure Initialise (argParentNode : in NodeObject_Pointer; argConnectionLimit : in Natural);
      entry addNodeConnection (Node : NodeObject_Pointer);
      procedure setOutputConnections (Outputs : OutputInterfaceArray_Pointer);
      
      -- Testing functions
      function AnyConnection return Targetable_Pointer;
      
   private
      Connections : ConnectionArray.Map;
      OutputConnections : OutputInterfaceArray_Pointer := null;
      ParentNode : NodeObject_Pointer;
      ConnectionLimit : Natural := 0;
   end ConnectionManagementType;
   type ConnectionManagement_Pointer is access all ConnectionManagementType;
   function getConnectionsObject (node : in NodeObject_Pointer) return ConnectionManagement_Pointer;
   
   
   
private 

   NewNodeIDIndex : NodeID := NodeID'First;
   
      -- Encapsulates inputs, connections and extension information and tasks
   type NodeObject is
      record
         
         -- Node identification
         id : NodeID;
         
         -- Main objects
         Data : DataManagement_Pointer;
         UnprocessedDataQueue : Data_FIFO_Pointer;
         Connections : ConnectionManagement_Pointer;
         FiringMechanism : FiringMechanism_Pointer;
         
         -- Extension tasks
         FiringCondition : FiringCondition_Pointer;
         TargetSelection : TargetSelection_Pointer;
         TransmissionContent : TransmissionContent_Pointer;
         DataProcessing : DataProcessing_Pointer;
         GeneticSequence : GeneticSequence_Pointer;
         EnergyEconomics : EnergyEconomics_Pointer;
         LifeCycle : LifeCycle_Pointer;
         
      end record;
   
   
end NetworkNode;
