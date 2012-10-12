package GoalsDomain is
   
   type Reward is new Float range -1 .. 1;
   Global_Reward : Reward := 0;
   
   procedure Initialise_GoalsDomain ();
   function GD_DetermineReward (id : InputInterfaceID; Data : Data_Pointer) return Reward;
   procedure DG_DetermineGlobalReward;
   
private
   
   DomainIntialised : Boolean;
   
end GoalsDomain;
