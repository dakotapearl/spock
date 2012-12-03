package experimentDomain.Binary;

import tools.errorChecking.Assert;
import networkDomain.NetworkNode;
import networkDomain.behaviours.neuralEcosystem.*;
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
		
		// TODO condense into form network operation
		addNodeTemplate("NeuralEcosystemTemplate", 
				        InputProcess.class, 
				        OutputProcess.class, 
				        StorageProcess.class, 
				        NeuralEcosystem_FC.class, 
				        NeuralEcosystem_TS.class, 
				        GeneticSequence.class, 
				        NeuralEcosystem_EE.class, 
				        NeuralEcosystem_LC.class, 
				        NeuralEcosystem_DP.class, 
				        NeuralEcosystem_TC.class);

		NetworkNode node = addNewNode("NeuralEcosystemTemplate");
		
		// Create environment
		BinaryANDEnvironment env = new BinaryANDEnvironment(experimentDomain.environmentDomain);
		addEnvironment(env);
		
		// TODO condense into an EnvironmentOutputInterface operation 
		env.getPerceptions().get(0).addObserver(node.inputProcess);
		
		// TODO condense into an EnvironmentInputInterface operation
		node.storageProcess.registerAction(env.getActions().get(0));
		node.storageProcess.registerAction(env.getActions().get(1));
	}

}