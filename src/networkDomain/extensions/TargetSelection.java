package networkDomain.extensions;

import networkDomain.NetworkComponent;
import networkDomain.NetworkSignal;

/**
 * Instantiated once for whole network
 * @author Loren
 *
 */
public interface TargetSelection {
	
	public void setNXE(NodeExtensionEncapsulator NXE);
	public NetworkComponent selectTarget(NetworkSignal signal);
	
}
