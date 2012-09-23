package networkDomain.extensions;

import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;

/**
 * Instantiated once for whole network
 * @author Loren
 *
 */
public interface TargetSelection {
	
	public void setNXE(NodeExtensionEncapsulator NXE);
	public NetworkNode selectTarget(NetworkSignal signal);
	
}
