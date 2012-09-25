package experimentDomain.TestExperiment;

import networkDomain.NetworkNode;
import networkDomain.core.*;
import experimentDomain.Experiment;
import experimentDomain.ExperimentDomain;
import experimentDomain.TestExperiment.environmentClasses.TestingSystem;
import experimentDomain.TestExperiment.networkClasses.*;

public class TestExperiment extends Experiment {

	public TestExperiment(ExperimentDomain experimentDomain) {
		super(experimentDomain);
	}

	@Override
	public void initialiseExperiment() {
		addNodeTemplate("TestTemplate", InputProcess.class, OutputProcess.class, StorageProcess.class, 
				                        fc.class, ts.class, gs.class, ee.class, lc.class, dp.class, tc.class);
		
		NetworkNode node = addNewNode("TestTemplate");
		
		TestingSystem env = new TestingSystem(experimentDomain.environmentDomain);
		addEnvironment(env);
		
		env.getPerceptions().get(0).addObserver(node.inputProcess);
		
		node.storageProcess.registerAction(env.getActions().get(0));
		
	}
	
}
