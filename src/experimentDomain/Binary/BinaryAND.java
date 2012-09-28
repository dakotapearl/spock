package experimentDomain.Binary;

import tools.errorChecking.Assert;
import networkDomain.NetworkNode;
import networkDomain.core.InputProcess;
import networkDomain.core.OutputProcess;
import networkDomain.core.StorageProcess;
import networkDomain.extensions.implementations.Test.dp;
import networkDomain.extensions.implementations.Test.ee;
import networkDomain.extensions.implementations.Test.fc;
import networkDomain.extensions.implementations.Test.gs;
import networkDomain.extensions.implementations.Test.lc;
import networkDomain.extensions.implementations.Test.tc;
import networkDomain.extensions.implementations.Test.ts;
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
                						fc.class, ts.class, gs.class, ee.class, lc.class, dp.class, tc.class);

		NetworkNode node = addNewNode("TestTemplate");
		
		BinaryANDEnvironment env = new BinaryANDEnvironment(experimentDomain.environmentDomain);
		addEnvironment(env);
		
		env.getPerceptions().get(0).addObserver(node.inputProcess);
		
		node.storageProcess.registerAction(env.getActions().get(0));
		node.storageProcess.registerAction(env.getActions().get(1));
	}

}
