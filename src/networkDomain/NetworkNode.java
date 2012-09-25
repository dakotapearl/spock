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
public class NetworkNode implements NetworkTargetable, NetworkTransmitter {
	
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
	
	public NetworkNode(InputProcess inputProcess,
					   OutputProcess outputProcess,
					   StorageProcess storageProcess,
					   FiringCondition firingCondition, 
					   TargetSelection targetSelection, 
					   GeneticSequence geneticSequence, 
					   EnergyEconomics energyEconomics, 
					   LifeCycle lifeCycle, 
					   DataProcessing dataProcessing, 
					   TransmissionContent transmissionContent) {
		// TODO assert none are null.
		
		this.inputProcess = inputProcess;
		this.outputProcess = outputProcess;
		this.storageProcess = storageProcess;
		
		this.firingCondition = firingCondition;
		this.targetSelection = targetSelection;
		this.geneticSequence = geneticSequence;
		this.energyEconomics = energyEconomics;
		this.lifeCycle = lifeCycle;
		this.dataProcessing = dataProcessing;
		this.transmissionContent = transmissionContent;
		
		inputProcess.declareParent(this);
		outputProcess.declareParent(this);
		storageProcess.declareParent(this);
		
		firingCondition.declareParent(this);
		targetSelection.declareParent(this);
		geneticSequence.declareParent(this);
		energyEconomics.declareParent(this);
		lifeCycle.declareParent(this);
		dataProcessing.declareParent(this);
		transmissionContent.declareParent(this);
		
	}
	
	public void acceptSignal(NetworkSignal signal, NetworkTransmitter sender) {
		inputProcess.acceptSignal(signal, sender);
	}
	
	public void start() {
		firingCondition.start();
		targetSelection.start();
		geneticSequence.start();
		energyEconomics.start();
		lifeCycle.start();
		dataProcessing.start();
		transmissionContent.start();
		
		inputProcess.start();
		outputProcess.start();
	}
	
	public NetworkNode replicateNode() {
		
		// Make new node and direct all vital functions to current node
		NetworkNode newNode = new NetworkNode(inputProcess, 
				                              outputProcess, 
				                              storageProcess, 
				                              firingCondition, 
				                              targetSelection, 
				                              geneticSequence, 
				                              energyEconomics, 
				                              lifeCycle, 
				                              dataProcessing, 
				                              transmissionContent);
		
		// Start new node
		// ?
		
		// Let node functions divide themselves and direct the new version to the new node
		
		
		
		return newNode;
	}
	
}
