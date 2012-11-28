package experimentDomain.Binary;

import tools.errorChecking.Assert;
import networkDomain.NetworkNode;
import networkDomain.behaviours.test.dp;
import networkDomain.behaviours.test.ee;
import networkDomain.behaviours.test.fc;
import networkDomain.behaviours.test.lc;
import networkDomain.behaviours.test.tc;
import networkDomain.behaviours.test.ts;
import networkDomain.core.InputProcess;
import networkDomain.core.OutputProcess;
import networkDomain.core.StorageProcess;
import networkDomain.core.GeneticSequence;
import environmentDomain.binary.AND.BinaryANDEnvironment;
import experimentDomain.Experiment;
import experimentDomain.ExperimentDomain;

/**
 * @author Loren Chorley
 */
public class BinaryAND extends Experiment {

	public BinaryAND(ExperimentDomain experimentDomain) {
		super(experimentDomain);
		Assert.AssertTrue("ExperimentDomain correctly passed to BinaryAND", experimentDomain != null);
	}

	@Override
	public void initialiseExperiment() {
		addNodeTemplate("TestTemplate", InputProcess.class, OutputProcess.class, StorageProcess.class, 
                						fc.class, ts.class, GeneticSequence.class, ee.class, lc.class, dp.class, tc.class);

		NetworkNode node = addNewNode("TestTemplate");
		
		BinaryANDEnvironment env = new BinaryANDEnvironment(experimentDomain.environmentDomain);
		addEnvironment(env);
		
		env.getPerceptions().get(0).addObserver(node.inputProcess);
		
		node.storageProcess.registerAction(env.getActions().get(0));
		node.storageProcess.registerAction(env.getActions().get(1));
	}

}