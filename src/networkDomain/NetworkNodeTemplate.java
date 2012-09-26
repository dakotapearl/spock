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
import tools.errorChecking.Assert;

public class NetworkNodeTemplate {
	
	public NetworkDomain networkDomain;
	
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
	private Class classGeneticSequence;
	@SuppressWarnings("rawtypes")
	private Class classEnergyEconomics;
	@SuppressWarnings("rawtypes")
	private Class classLifeCycle;
	@SuppressWarnings("rawtypes")
	private Class classDataProcessing;
	@SuppressWarnings("rawtypes")
	private Class classTransmissionContent;
	
	@SuppressWarnings("rawtypes")
	public NetworkNodeTemplate(NetworkDomain networkDomain,
							   Class classInputProcess,
			                   Class classOutputProcess,
			                   Class classStorageProcess,
			    		       Class classFiringCondition, 
			    		       Class classTargetSelection, 
			    		       Class classGeneticSequence, 
			    		       Class classEnergyEconomics, 
			    		       Class classLifeCycle, 
			    		       Class classDataProcessing, 
			    		       Class classTransmissionContent) {
		Assert.CriticalAssertTrue("Non-null classes were passed to NetworkNodeTemplate constructor", (classInputProcess != null) &&
																									 (classOutputProcess != null) &&	
																									 (classStorageProcess != null) &&
																									 (classFiringCondition != null) &&
																									 (classTargetSelection != null) &&
																							 		 (classGeneticSequence != null) &&
																							 		 (classEnergyEconomics != null) &&
																							 		 (classDataProcessing != null) &&
																							 		 (classLifeCycle != null) &&
																							 		 (classTransmissionContent != null));
		
		Assert.AssertTrue("NetworkDomain correctly passed to NetworkNodeTemplate", networkDomain != null);
		
		this.networkDomain = networkDomain;
		
		this.classInputProcess = classInputProcess;
		this.classOutputProcess = classOutputProcess;
		this.classStorageProcess = classStorageProcess;
		
		this.classFiringCondition = classFiringCondition;
		this.classTargetSelection = classTargetSelection;
		this.classGeneticSequence = classGeneticSequence;
		this.classEnergyEconomics = classEnergyEconomics;
		this.classLifeCycle = classLifeCycle;
		this.classDataProcessing = classDataProcessing;
		this.classTransmissionContent = classTransmissionContent;

	}
	
	public NetworkNode newInstance() {
		return new NetworkNode(networkDomain,
							   newInputProcess(),
							   newOutputProcess(),
							   newStorageProcess(),
				               newFiringCondition(), 
				               newTargetSelection(), 
				               newGeneticSequence(), 
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
	
	private GeneticSequence newGeneticSequence() {
		Object e = instantiateClass(classGeneticSequence);	
		Assert.CriticalAssertTrue("Is correct object: GeneticSequence", e instanceof GeneticSequence);
		return (GeneticSequence) e;
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
			System.exit(1);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}
	
}
