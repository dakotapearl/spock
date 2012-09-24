package experimentDomain.TestExperiment.networkClasses;

import networkDomain.extensions.LifeCycle;
import networkDomain.extensions.NodeExtensionEncapsulator;

public class lc implements LifeCycle {

	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}

}
