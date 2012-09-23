package experimentDomain;

import tools.Assert;
import networkDomain.extensions.*;

public class NXETemplate {
	
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
	public NXETemplate(Class classFiringCondition, Class classTargetSelection, Class classGeneticSequence, Class classEnergyEconomics, Class classLifeCycle, Class classDataProcessing, Class classTransmissionContent) {
		Assert.CriticalAssertTrue("Non-null classes were passed to NXETemplate constructor", (classFiringCondition != null) &&
																							 (classTargetSelection != null) &&
																							 (classGeneticSequence != null) &&
																							 (classEnergyEconomics != null) &&
																							 (classDataProcessing != null) &&
																							 (classLifeCycle != null) &&
																							 (classTransmissionContent != null));
		
		this.classFiringCondition = classFiringCondition;
		this.classTargetSelection = classTargetSelection;
		this.classGeneticSequence = classGeneticSequence;
		this.classEnergyEconomics = classEnergyEconomics;
		this.classLifeCycle = classLifeCycle;
		this.classDataProcessing = classDataProcessing;
		this.classTransmissionContent = classTransmissionContent;

	}
	
	public NodeExtensionEncapsulator newInstance() {
		return new NodeExtensionEncapsulator(newFiringCondition(), 
				                             newTargetSelection(), 
				                             newGeneticSequence(), 
				                             newEnergyEconomics(), 
				                             newLifeCycle(), 
				                             newDataProcessing(), 
				                             newTransmissionContent());
	}
	
	private FiringCondition newFiringCondition() {
		Object e = instantiateExtension(classFiringCondition);	
		Assert.CriticalAssertTrue("Is correct extension: FiringCondition", e instanceof FiringCondition);
		return (FiringCondition) e;
	}
	
	private TargetSelection newTargetSelection() {
		Object e = instantiateExtension(classTargetSelection);	
		Assert.CriticalAssertTrue("Is correct extension: TargetSelection", e instanceof TargetSelection);
		return (TargetSelection) e;
	}
	
	private GeneticSequence newGeneticSequence() {
		Object e = instantiateExtension(classGeneticSequence);	
		Assert.CriticalAssertTrue("Is correct extension: GeneticSequence", e instanceof GeneticSequence);
		return (GeneticSequence) e;
	}
	
	private EnergyEconomics newEnergyEconomics() {
		Object e = instantiateExtension(classEnergyEconomics);	
		Assert.CriticalAssertTrue("Is correct extension: EnergyEconomics", e instanceof EnergyEconomics);
		return (EnergyEconomics) e;
	}
	
	private LifeCycle newLifeCycle() {
		Object e = instantiateExtension(classLifeCycle);	
		Assert.CriticalAssertTrue("Is correct extension: LifeCycle", e instanceof LifeCycle);
		return (LifeCycle) e;
	}
	
	private DataProcessing newDataProcessing() {
		Object e = instantiateExtension(classDataProcessing);	
		Assert.CriticalAssertTrue("Is correct extension: DataProcessing", e instanceof DataProcessing);
		return (DataProcessing) e;
	}
	
	private TransmissionContent newTransmissionContent() {
		Object e = instantiateExtension(classTransmissionContent);	
		Assert.CriticalAssertTrue("Is correct extension: TransmissionContent", e instanceof TransmissionContent);
		return (TransmissionContent) e;
	}
	
	private Object instantiateExtension(@SuppressWarnings("rawtypes") Class ext) {
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