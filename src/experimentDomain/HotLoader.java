package experimentDomain;

/**
 * @author Loren Chorley
 */
public class HotLoader {
	
	public void loadBehaviourClassesToTemplate(String templateName, String extensionsPackage, String extensionsPrefix, Experiment experiment) throws InvalidPackageName, ExtensionNotFound {
		// make own exception
		// load class* by package names
		// classFiringCondition = getClassOf("networkDomain.extensions.implementation." + extensionsName + "." + extensionsPrefix + "_FC")
		// Check for the existence of input, output and storage. If none, use the default implementations. Extensions are requred.
		// experiment.addNodeTemplate(newTemplateName, classInputProcess, classOutputProcess, classStorageProcess, classFiringCondition, classTargetSelection, classGeneticSequence, classEnergyEconomics, classLifeCycle, classDataProcessing, classTransmissionContent)
	}
	
}
