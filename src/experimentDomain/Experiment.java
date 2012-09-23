package experimentDomain;

import java.util.ArrayList;
import java.util.HashMap;
import tools.Assert;
import networkDomain.NetworkNode;
import networkDomain.extensions.NodeExtensionEncapsulator;
import environmentDomain.SensoryMotorSystem;

public abstract class Experiment {
	
	private ArrayList<SensoryMotorSystem> environments;
	private NetworkNode network;
	private HashMap<String, NXETemplate> NXETemplates;
	
	public Experiment() {
		environments = new ArrayList<SensoryMotorSystem>();
		NXETemplates = new HashMap<String, NXETemplate>();
		tools.Log.created(this.getClass());
	}
	
	@SuppressWarnings("unchecked")
	public void addNXETemplate(String templateIdenifier, Class classFiringCondition, Class classTargetSelection, Class classGeneticSequence, Class classEnergyEconomics, Class classCellularLifeCycle, Class classDataProcessing, Class classTransmissionContent) {
		Assert.CriticalAssertTrue("NXETemplate identifier is valid", (templateIdenifier != null) && !templateIdenifier.equals(""));

		NXETemplate newTemplate = new NXETemplate(classFiringCondition, classTargetSelection, classGeneticSequence, classEnergyEconomics, classCellularLifeCycle, classDataProcessing, classTransmissionContent);
		NXETemplates.put(templateIdenifier, newTemplate);
	}
	
	public NodeExtensionEncapsulator newNXE(String templateIdentifier) {
		Assert.CriticalAssertTrue("Valid NXE template key was passed to newNXE", NXETemplates.containsKey(templateIdentifier));
		
		return NXETemplates.get(templateIdentifier).newInstance();
	}
	
	public void addEnvironment(SensoryMotorSystem environment) {
		environments.add(environment);
	}
	
	
	public ArrayList<SensoryMotorSystem> getEnvironments() {
		return environments;
	}
	
	public void readyExperiment() {
		Assert.CriticalAssertTrue("All required experiment variables set", (NXETemplates.size() > 0) &&
				 														   (network != null) &&
				 														   (environments.size() > 0));
		
		for (SensoryMotorSystem sms : environments)
			sms.activatePerceptions();
	}
	
}
