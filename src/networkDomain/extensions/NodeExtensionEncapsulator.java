package networkDomain.extensions;

public class NodeExtensionEncapsulator {

	public FiringCondition firingCondition;
	public TargetSelection targetSelection;
	public GeneticSequence geneticSequence;
	public EnergyEconomics energyEconomics;
	public LifeCycle lifeCycle;
	public DataProcessing dataProcessing;
	public TransmissionContent transmissionContent;
	
	public NodeExtensionEncapsulator(FiringCondition firingCondition, TargetSelection targetSelection, GeneticSequence geneticSequence, EnergyEconomics energyEconomics, LifeCycle lifeCycle, DataProcessing dataProcessing, TransmissionContent transmissionContent) {
		// TODO assert none are null.
		
		this.firingCondition = firingCondition;
		this.targetSelection = targetSelection;
		this.geneticSequence = geneticSequence;
		this.energyEconomics = energyEconomics;
		this.lifeCycle = lifeCycle;
		this.dataProcessing = dataProcessing;
		this.transmissionContent = transmissionContent;
		
		firingCondition.setNXE(this);
		targetSelection.setNXE(this);
		geneticSequence.setNXE(this);
		energyEconomics.setNXE(this);
		lifeCycle.setNXE(this);
		dataProcessing.setNXE(this);
		transmissionContent.setNXE(this);
		
	}

}
