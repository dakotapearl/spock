package experimentsDomain.test;

import tools.Log;
import environmentsDomain.Perception;
import environmentsDomain.SensoryMotorSystem;
import experimentsDomain.Experiment;
import experimentsDomain.ExperimentsDomain;
import experimentsDomain.test.extensions.*;
import networkDomain.NetworkConfiguration;
import networkDomain.NetworkNode;
import networkDomain.extensions.*;
import networkDomain.visitors.VisitorFillLayer;

public class Config_Test implements NetworkConfiguration {

	@Override
	public void buildNetwork(ExperimentsDomain experimentsDomain) {
		
		Experiment experiment = experimentsDomain.getExperiment();
		
		Log.write("Configuration (1): making environment");
		experiment.addEnvironment(experimentsDomain.getEnvironmentsDomain().newSensoryMotorSystem("testing.TestingSystem"));
		SensoryMotorSystem environment = experiment.getEnvironments().get(0);
		
		Log.write("Configuration (2): setting extensions");
		experiment.addNXETemplate("Test Template", fc.class, 
												   ts.class, 
												   gs.class, 
												   ee.class, 
												   clc.class, 
												   dp.class, 
												   tc.class);
				
		NodeExtensionEncapsulator rootNodeNXE = experiment.newNXE("Test Template"); 
		
		Log.write("Configuration (3): building network root node");
		experiment.setNetwork(experimentsDomain.getNetworkDomain().newNetwork(rootNodeNXE));
		NetworkNode network = experiment.getNetwork();
		
		Log.write("Configuration (4): building network");
		VisitorFillLayer Layerer = new VisitorFillLayer(experimentsDomain.getNetworkDomain());
		
		Layerer.setNumberOfInputNodes(1);
		Layerer.setNumberOfOutputNodes(1);
		Layerer.setNumberOfOrdinaryNodes(1);
		Layerer.setNXETemplate("Test Template");
		
		network.acceptVisitor(Layerer); // First layer
		network.acceptVisitor(Layerer); // Second layer (built to the same specifications)
		
		Log.write("Configuration (5): attaching network to environment");
		
		// Needs to be abstracted away
		for (NetworkNode n : network.getOutputChildrenNodes())
			n.registerActions(environment.getActions());
		
		// Needs to be abstracted away
		for (Perception p : environment.getPerceptions())
			p.registerSensors(network.getInputChildrenNodes());
		
	}
}
