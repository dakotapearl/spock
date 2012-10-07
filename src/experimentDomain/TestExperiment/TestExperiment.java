package experimentDomain.TestExperiment;

import networkDomain.NetworkNode;
import networkDomain.behaviours.test.dp;
import networkDomain.behaviours.test.ee;
import networkDomain.behaviours.test.fc;
import networkDomain.behaviours.test.lc;
import networkDomain.behaviours.test.tc;
import networkDomain.behaviours.test.ts;
import networkDomain.core.*;
import networkDomain.extensions.implementations.Test.gs;
import environmentDomain.TestEnvironment.TestingSystem;
import experimentDomain.Experiment;
import experimentDomain.ExperimentDomain;

/**
 * @author Loren Chorley
 */
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
