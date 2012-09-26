package experimentDomain;

import java.util.ArrayList;
import java.util.HashMap;
import tools.errorChecking.Assert;
import networkDomain.Network;
import networkDomain.NetworkNode;
import networkDomain.NetworkNodeTemplate;
import environmentDomain.Environment;
import environmentDomain.EnvironmentDomain;

public abstract class Experiment {
	
	public ExperimentDomain experimentDomain;
	private ArrayList<Environment> environments;
	private Network network;
	private HashMap<String, NetworkNodeTemplate> networkNodeTemplates;
	
	public Experiment(ExperimentDomain experimentDomain) {
		Assert.AssertTrue("ExperimentDomain correctly passed to Experiment", experimentDomain != null);
		
		this.experimentDomain = experimentDomain;
		environments = new ArrayList<Environment>();
		networkNodeTemplates = new HashMap<String, NetworkNodeTemplate>();
		network = new Network();
		
		initialiseExperiment();
		
		tools.errorChecking.Log.created(this.getClass());
	}
	
	@SuppressWarnings({ "rawtypes" })
	public void addNodeTemplate(String templateIdenifier, Class classInputProcess,
                                                          Class classOutputProcess,
                                                          Class classStorageProcess,
			                                              Class classFiringCondition, 
			                                              Class classTargetSelection, 
			                                              Class classGeneticSequence, 
			                                              Class classEnergyEconomics, 
			                                              Class classLifeCycle, 
			                                              Class classDataProcessing, 
			                                              Class classTransmissionContent) {
		Assert.CriticalAssertTrue("NetworkNodeTemplate identifier is valid", (templateIdenifier != null) && !templateIdenifier.equals(""));

		NetworkNodeTemplate newTemplate = new NetworkNodeTemplate(experimentDomain.networkDomain,
																  classInputProcess, 
												  				  classOutputProcess, 
												  				  classStorageProcess, 
												  				  classFiringCondition, 
												  				  classTargetSelection, 
												  				  classGeneticSequence, 
												  				  classEnergyEconomics, 
												  				  classLifeCycle, 
												  				  classDataProcessing, 
												  				  classTransmissionContent);
		networkNodeTemplates.put(templateIdenifier, newTemplate);
	}
	
	public NetworkNode addNewNode(String templateIdentifier) {
		Assert.CriticalAssertTrue("Valid node template key was passed to newNode", networkNodeTemplates.containsKey(templateIdentifier));
		
		NetworkNode n = networkNodeTemplates.get(templateIdentifier).newInstance();
		network.nodes.add(n);
		
		return n;
	}
	
	public void addEnvironment(Environment environment) {
		environments.add(environment);
	}
	
	public ArrayList<Environment> getEnvironments() {
		return environments;
	}
	
	public void start() {
		Assert.CriticalAssertTrue("All required experiment variables set", (networkNodeTemplates.size() > 0) &&
				 														   (network != null) &&
				 														   (environments.size() > 0));
		
		network.start();
		
		for (Environment env : environments)
			env.start();
		
	}
	
	public abstract void initialiseExperiment();
	
}
