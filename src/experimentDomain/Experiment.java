package experimentDomain;

import interfaceDomain.InterfaceObservable;

import java.util.ArrayList;
import java.util.HashMap;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;
import networkDomain.Network;
import networkDomain.NetworkNode;
import networkDomain.NetworkNodeTemplate;
import environmentDomain.Environment;

/**
 * @author Loren Chorley
 */
public abstract class Experiment extends Thread {
	
	public ExperimentDomain experimentDomain;
	private ArrayList<Environment> environments;
	private Network network;
	private HashMap<String, NetworkNodeTemplate> networkNodeTemplates;
	public HashMap<String, InterfaceObservable> interfaceObservables;
	
	public Experiment(ExperimentDomain experimentDomain) {
		Assert.AssertTrue("ExperimentDomain correctly passed to Experiment", experimentDomain != null);
		
		this.experimentDomain = experimentDomain;
		environments = new ArrayList<Environment>();
		networkNodeTemplates = new HashMap<String, NetworkNodeTemplate>();
		
		System.out.println(experimentDomain.toString()); //TODO exp dom not properly passed
		
		network = new Network(experimentDomain.networkDomain);
		experimentDomain.networkDomain.setNetwork(network);
		interfaceObservables = new HashMap<String, InterfaceObservable>();
		
		// TODO Initialise interface variables
		
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
	
	@Override
	public void run() {
		Assert.CriticalAssertTrue("All required experiment variables set", (networkNodeTemplates.size() > 0) &&
				 														   (network != null) &&
				 														   (environments.size() > 0));
		
		Log.writeForThreadCreation("Experiment");
		Assert.CriticalAssertTrue("Experiment thread started", this.isAlive());
		
		network.start();
		
		for (Environment env : environments)
			env.start();
		
	}
	
	public abstract void initialiseExperiment();
	
}
