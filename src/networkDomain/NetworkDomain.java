package networkDomain;

import tools.Log;
import networkDomain.extensions.NodeExtensionEncapsulator;
import dataDomain.DataClientInterface;
import dataDomain.DataDomain;

public class NetworkDomain {
	
	public void initialise() {
		Log.write("Network domain initialised");
	}
	
	private DataClientInterface dataEE;
	
	public void setDataEE(DataClientInterface dataEE) {
		this.dataEE = dataEE;
	}
	
	public DataDomain getDataDomain() {
		return (DataDomain) dataEE;
	}

	NetworkNode network;
	
	public NetworkNode getNetwork() {
		return network;
	}
	
	public NetworkNode newNetwork(NodeExtensionEncapsulator NXE) {
		network = new NetworkNode();
		return network;
	}
	
}
