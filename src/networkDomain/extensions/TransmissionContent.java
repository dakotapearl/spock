package networkDomain.extensions;

import networkDomain.NetworkSignal;

public interface TransmissionContent {
	
	public void setNXE(NodeExtensionEncapsulator NXE);
	public NetworkSignal nextSignalToFire();
	public boolean signalsRemain();
	
}
