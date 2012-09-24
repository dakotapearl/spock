package networkDomain;

import networkDomain.core.InputProcess;
import networkDomain.core.OutputProcess;
import networkDomain.core.StorageProcess;
import networkDomain.extensions.DataProcessing;
import networkDomain.extensions.EnergyEconomics;
import networkDomain.extensions.FiringCondition;
import networkDomain.extensions.GeneticSequence;
import networkDomain.extensions.LifeCycle;
import networkDomain.extensions.TargetSelection;
import networkDomain.extensions.TransmissionContent;

/**
 * 
 * 
 * @author Loren Chorley
 *
 */
public class NetworkNode {
	
	public InputProcess inputProcess;
	public OutputProcess outputProcess;
	public StorageProcess storageProcess;
	public FiringCondition firingCondition;
	public TargetSelection targetSelection;
	public GeneticSequence geneticSequence;
	public EnergyEconomics energyEconomics;
	public LifeCycle lifeCycle;
	public DataProcessing dataProcessing;
	public TransmissionContent transmissionContent;
	
	public NetworkNode(FiringCondition firingCondition, 
					   TargetSelection targetSelection, 
					   GeneticSequence geneticSequence, 
					   EnergyEconomics energyEconomics, 
					   LifeCycle lifeCycle, 
					   DataProcessing dataProcessing, 
					   TransmissionContent transmissionContent) {
		// TODO assert none are null.
		
		this.firingCondition = firingCondition;
		this.targetSelection = targetSelection;
		this.geneticSequence = geneticSequence;
		this.energyEconomics = energyEconomics;
		this.lifeCycle = lifeCycle;
		this.dataProcessing = dataProcessing;
		this.transmissionContent = transmissionContent;
		
		firingCondition.declareParent(this);
		targetSelection.declareParent(this);
		geneticSequence.declareParent(this);
		energyEconomics.declareParent(this);
		lifeCycle.declareParent(this);
		dataProcessing.declareParent(this);
		transmissionContent.declareParent(this);
		
	}
	
	// Consider moving entire NXE functionality here
	
}
