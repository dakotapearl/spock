package experimentDomain.Binary;

import networkDomain.NetworkNode;
import networkDomain.core.InputProcess;
import networkDomain.core.OutputProcess;
import networkDomain.core.StorageProcess;
import environmentDomain.binary.AND.BinaryANDEnvironment;
import experimentDomain.Experiment;
import experimentDomain.ExperimentDomain;
import experimentDomain.TestExperiment.networkClasses.dp;
import experimentDomain.TestExperiment.networkClasses.ee;
import experimentDomain.TestExperiment.networkClasses.fc;
import experimentDomain.TestExperiment.networkClasses.gs;
import experimentDomain.TestExperiment.networkClasses.lc;
import experimentDomain.TestExperiment.networkClasses.tc;
import experimentDomain.TestExperiment.networkClasses.ts;

public class BinaryAND extends Experiment {

	public BinaryAND(ExperimentDomain experimentDomain) {
		super(experimentDomain);
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
