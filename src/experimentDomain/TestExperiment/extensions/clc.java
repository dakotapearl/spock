package experimentsDomain.test.extensions;

import networkDomain.extensions.CellularLifeCycle;
import networkDomain.extensions.NodeExtensionEncapsulator;

public class clc implements CellularLifeCycle {

	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}

}
