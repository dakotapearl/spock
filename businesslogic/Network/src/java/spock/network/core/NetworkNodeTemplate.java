package spock.network.core;

import spock.network.behaviours.*;
import tools.errorChecking.Assert;

/**
 * Allows for the creation of an arbitrary number of nodes with the same behaviour
 * @author Loren Chorley
 */
public class NetworkNodeTemplate {
	
    public Network network;

    @SuppressWarnings("rawtypes")
    private Class classInputProcess;
    @SuppressWarnings("rawtypes")
    private Class classOutputProcess;
    @SuppressWarnings("rawtypes")
    private Class classStorageProcess;

    @SuppressWarnings("rawtypes")
    private Class classFiringCondition;
    @SuppressWarnings("rawtypes")
    private Class classTargetSelection;
    @SuppressWarnings("rawtypes")
    private Class classNodeProperties;
    @SuppressWarnings("rawtypes")
    private Class classEnergyEconomics;
    @SuppressWarnings("rawtypes")
    private Class classLifeCycle;
    @SuppressWarnings("rawtypes")
    private Class classDataProcessing;
    @SuppressWarnings("rawtypes")
    private Class classTransmissionContent;

    @SuppressWarnings("rawtypes")
    public NetworkNodeTemplate(Network network,
                               Class classInputProcess,
                               Class classOutputProcess,
                               Class classStorageProcess,
                               Class classNodeProperties, 
                               Class classFiringCondition, 
                               Class classTargetSelection, 
                               Class classEnergyEconomics, 
                               Class classLifeCycle, 
                               Class classDataProcessing, 
                               Class classTransmissionContent) {

        assert((classInputProcess != null) &&
               (classOutputProcess != null) &&	
               (classStorageProcess != null) &&
               (classFiringCondition != null) &&
               (classTargetSelection != null) &&
               (classNodeProperties != null) &&
               (classEnergyEconomics != null) &&
               (classDataProcessing != null) &&
               (classLifeCycle != null) &&
               (classTransmissionContent != null))
                : "Non-null classes were passed to NetworkNodeTemplate constructor";

        Assert.AssertTrue("Network correctly passed to NetworkNodeTemplate", network != null);

        this.network = network;

        this.classInputProcess = classInputProcess;
        this.classOutputProcess = classOutputProcess;
        this.classStorageProcess = classStorageProcess;
        this.classNodeProperties = classNodeProperties;
        this.classFiringCondition = classFiringCondition;
        this.classTargetSelection = classTargetSelection;
        this.classEnergyEconomics = classEnergyEconomics;
        this.classLifeCycle = classLifeCycle;
        this.classDataProcessing = classDataProcessing;
        this.classTransmissionContent = classTransmissionContent;

    }

    public NetworkNode newInstance() {
        return new NetworkNode(network,
                               newInputProcess(),
                               newOutputProcess(),
                               newStorageProcess(),
                               newNodeProperties(),                
                               newFiringCondition(), 
                               newTargetSelection(), 
                               newEnergyEconomics(), 
                               newLifeCycle(), 
                               newDataProcessing(), 
                               newTransmissionContent());
    }

    private InputProcess newInputProcess() {
        Object e = instantiateClass(classInputProcess);	
        Assert.CriticalAssertTrue("Is correct object: InputProcess", e instanceof InputProcess);
        return (InputProcess) e;
    }

    private OutputProcess newOutputProcess() {
        Object e = instantiateClass(classOutputProcess);	
        Assert.CriticalAssertTrue("Is correct object: OutputProcess", e instanceof OutputProcess);
        return (OutputProcess) e;
    }

    private StorageProcess newStorageProcess() {
        Object e = instantiateClass(classStorageProcess);	
        Assert.CriticalAssertTrue("Is correct object: StorageProcess", e instanceof StorageProcess);
        return (StorageProcess) e;
    }

    private FiringCondition newFiringCondition() {
        Object e = instantiateClass(classFiringCondition);	
        Assert.CriticalAssertTrue("Is correct object: FiringCondition", e instanceof FiringCondition);
        return (FiringCondition) e;
    }

    private TargetSelection newTargetSelection() {
        Object e = instantiateClass(classTargetSelection);	
        Assert.CriticalAssertTrue("Is correct object: TargetSelection", e instanceof TargetSelection);
        return (TargetSelection) e;
    }

    private NodeProperties newNodeProperties() {
        Object e = instantiateClass(classNodeProperties);	
        Assert.CriticalAssertTrue("Is correct object: NodeProperties", e instanceof NodeProperties);
        return (NodeProperties) e;
    }

    private EnergyEconomics newEnergyEconomics() {
        Object e = instantiateClass(classEnergyEconomics);	
        Assert.CriticalAssertTrue("Is correct object: EnergyEconomics", e instanceof EnergyEconomics);
        return (EnergyEconomics) e;
    }

    private LifeCycle newLifeCycle() {
        Object e = instantiateClass(classLifeCycle);	
        Assert.CriticalAssertTrue("Is correct object: LifeCycle", e instanceof LifeCycle);
        return (LifeCycle) e;
    }

    private DataProcessing newDataProcessing() {
        Object e = instantiateClass(classDataProcessing);	
        Assert.CriticalAssertTrue("Is correct object: DataProcessing", e instanceof DataProcessing);
        return (DataProcessing) e;
    }

    private TransmissionContent newTransmissionContent() {
        Object e = instantiateClass(classTransmissionContent);	
        Assert.CriticalAssertTrue("Is correct object: TransmissionContent", e instanceof TransmissionContent);
        return (TransmissionContent) e;
    }

    private Object instantiateClass(@SuppressWarnings("rawtypes") Class ext) {
        try {
            Object o = ext.newInstance();
            return o;
        } catch (InstantiationException e) {
            e.printStackTrace();
            System.exit(1); // TODO Modify to be compatible with EJB framework
        } catch (IllegalAccessException e) {
            e.printStackTrace();
            System.exit(1);
        }
        return null;
    }

}
