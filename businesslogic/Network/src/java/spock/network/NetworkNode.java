package spock.network;

import interfaceDomain.InterfaceObservable;

import java.util.HashMap;

import tools.errorChecking.Assert;
import networkDomain.core.GeneticSequence;
import networkDomain.core.InputProcess;
import networkDomain.core.OutputProcess;
import networkDomain.core.StorageProcess;
import networkDomain.extensions.DataProcessing;
import networkDomain.extensions.EnergyEconomics;
import networkDomain.extensions.FiringCondition;
import networkDomain.extensions.LifeCycle;
import networkDomain.extensions.TargetSelection;
import networkDomain.extensions.TransmissionContent;

/**
 * @author Loren Chorley
 */
public class NetworkNode implements NetworkTargetable, NetworkTransmitter {
	
	private int id;
	
	public NetworkDomain networkDomain;
	
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
	
	public HashMap<String, InterfaceObservable> interfaceObservables;
	
	public NetworkNode(Integer id,
					   NetworkDomain networkDomain,
			           InputProcess inputProcess,
					   OutputProcess outputProcess,
					   StorageProcess storageProcess,
					   FiringCondition firingCondition, 
					   TargetSelection targetSelection, 
					   GeneticSequence geneticSequence, 
					   EnergyEconomics energyEconomics, 
					   LifeCycle lifeCycle, 
					   DataProcessing dataProcessing, 
					   TransmissionContent transmissionContent) {
		Assert.CriticalAssertTrue("Non-null classes were passed to NetworkNode", (inputProcess != null) &&
				 																 (outputProcess != null) &&	
				 																 (storageProcess != null) &&
				 																 (firingCondition != null) &&
				 																 (targetSelection != null) &&
																				 (geneticSequence != null) &&
																				 (energyEconomics != null) &&
																				 (dataProcessing != null) &&
																				 (lifeCycle != null) &&
																				 (transmissionContent != null));
		Assert.AssertTrue("NetworkDomain correctly passed to NetworkNode", networkDomain != null);
		
		
		this.id = id;
		this.networkDomain = networkDomain;
		
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
		
		interfaceObservables = new HashMap<String, InterfaceObservable>();
		
		// Initialise interface variables
		
		
	}
	
	public int getID() {
		return id;
	}
	
	public void acceptSignal(NetworkSignal signal, NetworkTransmitter sender) {
		inputProcess.acceptSignal(signal, sender);
	}
	
	public void start() {
		firingCondition.start();
		targetSelection.start();
		energyEconomics.start();
		lifeCycle.start();
		dataProcessing.start();
		transmissionContent.start();
		
		inputProcess.start();
		outputProcess.start();
	}
	
	public NetworkNode replicateNode() {
		
		// Make new node and direct all vital functions to current node
		NetworkNode newNode = new NetworkNode(networkDomain.getUniqueNodeID(),
											  networkDomain,
				                              inputProcess, 
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
		newNode.start();
		
		// Let node functions divide themselves and direct the new version to the new node
		inputProcess.replicateFunction(newNode.inputProcess);
		outputProcess.replicateFunction(newNode.outputProcess);
		storageProcess.replicateFunction(newNode.storageProcess);
		
		firingCondition.replicateFunction(newNode.firingCondition);
		targetSelection.replicateFunction(newNode.targetSelection);
		geneticSequence.replicateFunction(newNode.geneticSequence);
		energyEconomics.replicateFunction(newNode.energyEconomics);
		lifeCycle.replicateFunction(newNode.lifeCycle);
		dataProcessing.replicateFunction(newNode.dataProcessing);
		transmissionContent.replicateFunction(newNode.transmissionContent);
		
		return newNode;
	}
	
	public void pauseNode() {
		
	}
	
	public void resumeNode() {
		
	}
	
}
