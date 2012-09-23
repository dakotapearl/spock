package experimentDomain.TestExperiment.extensions;

import networkDomain.extensions.GeneticSequence;
import networkDomain.extensions.NodeExtensionEncapsulator;

public class gs implements GeneticSequence {

	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}

}
