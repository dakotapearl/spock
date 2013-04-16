package spock.network.core;

import spock.network.signals.NetworkSignal;
import spock.network.behaviours.*;

/**
 * @author Loren Chorley
 */
public class NetworkNode implements NetworkRunnable {
	
    private long id;

    public Network network;

    public InputProcess inputProcess;
    public OutputProcess outputProcess;
    public StorageProcess storageProcess;
    public NodeProperties nodeProperties;
    public FiringCondition firingCondition;
    public TargetSelection targetSelection;
    public EnergyEconomics energyEconomics;
    public LifeCycle lifeCycle;
    public DataProcessing dataProcessing;
    public TransmissionContent transmissionContent;

    public NetworkNode(Network network,
                       InputProcess inputProcess,
                       OutputProcess outputProcess,
                       StorageProcess storageProcess,
                       NodeProperties nodeProperties, 
                       FiringCondition firingCondition, 
                       TargetSelection targetSelection, 
                       EnergyEconomics energyEconomics, 
                       LifeCycle lifeCycle, 
                       DataProcessing dataProcessing, 
                       TransmissionContent transmissionContent) {

        assert((inputProcess != null) &&
               (outputProcess != null) &&	
               (storageProcess != null) &&
               (firingCondition != null) &&
               (targetSelection != null) &&
               (nodeProperties != null) &&
               (energyEconomics != null) &&
               (dataProcessing != null) &&
               (lifeCycle != null) &&
               (transmissionContent != null))
                : "Non-null classes were passed to NetworkNode";

        assert(network != null) : "Network correctly passed to NetworkNode";

        this.id = network.getUniqueNodeID();
        this.network = network;

        this.inputProcess = inputProcess;
        this.outputProcess = outputProcess;
        this.storageProcess = storageProcess;
        this.nodeProperties = nodeProperties;
        this.firingCondition = firingCondition;
        this.targetSelection = targetSelection;
        this.energyEconomics = energyEconomics;
        this.lifeCycle = lifeCycle;
        this.dataProcessing = dataProcessing;
        this.transmissionContent = transmissionContent;

        inputProcess.setParentNode(this);
        outputProcess.setParentNode(this);
        storageProcess.setParentNode(this);
        nodeProperties.setParentNode(this);
        firingCondition.setParentNode(this);
        targetSelection.setParentNode(this);
        energyEconomics.setParentNode(this);
        lifeCycle.setParentNode(this);
        dataProcessing.setParentNode(this);
        transmissionContent.setParentNode(this);

        // Initialise interface variables


    }

    public long getID() {
        return id;
    }

    public void acceptSignal(NetworkSignal signal, NetworkNode sender) {
        inputProcess.acceptSignal(signal, sender);
    }

    @Override
    public void start() {
        firingCondition.start();
        targetSelection.start();
        energyEconomics.start();
        lifeCycle.start();
        dataProcessing.start();
        transmissionContent.start();
        nodeProperties.start();
        storageProcess.start();
        inputProcess.start();
        outputProcess.start();
    }

    public NetworkNode replicateNode() {

        // Make new node and direct all vital functions to current node
        NetworkNode newNode = new NetworkNode(network,
                                              inputProcess, 
                                              outputProcess, 
                                              storageProcess, 
                                              nodeProperties,                               
                                              firingCondition, 
                                              targetSelection, 
                                              energyEconomics, 
                                              lifeCycle, 
                                              dataProcessing, 
                                              transmissionContent);

        // Start new node
        newNode.start();

        // Let node functions divide themselves and direct the new version to the new node
        inputProcess.replicateFunction(newNode.inputProcess, newNode);
        outputProcess.replicateFunction(newNode.outputProcess, newNode);
        storageProcess.replicateFunction(newNode.storageProcess, newNode);
        nodeProperties.replicateFunction(newNode.nodeProperties, newNode);
        firingCondition.replicateFunction(newNode.firingCondition, newNode);
        targetSelection.replicateFunction(newNode.targetSelection, newNode);
        energyEconomics.replicateFunction(newNode.energyEconomics, newNode);
        lifeCycle.replicateFunction(newNode.lifeCycle, newNode);
        dataProcessing.replicateFunction(newNode.dataProcessing, newNode);
        transmissionContent.replicateFunction(newNode.transmissionContent, newNode);

        return newNode;
    }

    /*Consider moving these functions from here and NetworkBehaviour
    to a common interface along with some other fuctions
    
    Also consider consolidating all behaviours into one class
    with the same general functions so that NewtorkNode
    can operate on them without loarge chunks of similar code*/
    
    @Override
    public void pauseActivity() {
        firingCondition.pauseActivity();
        targetSelection.pauseActivity();
        energyEconomics.pauseActivity();
        lifeCycle.pauseActivity();
        dataProcessing.pauseActivity();
        transmissionContent.pauseActivity();
        nodeProperties.pauseActivity();
        storageProcess.pauseActivity();
        inputProcess.pauseActivity();
        outputProcess.pauseActivity();
    }

    @Override
    public void resumeActivity() {
        firingCondition.resumeActivity();
        targetSelection.resumeActivity();
        energyEconomics.resumeActivity();
        lifeCycle.resumeActivity();
        dataProcessing.resumeActivity();
        transmissionContent.resumeActivity();
        nodeProperties.resumeActivity();
        storageProcess.resumeActivity();
        inputProcess.resumeActivity();
        outputProcess.resumeActivity();
    }

}
