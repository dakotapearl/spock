package networkDomain.extensions;

/**
 * Instantiated for each node
 * @author Loren
 *
 */
public interface FiringCondition {
	
	public void setNXE(NodeExtensionEncapsulator NXE);
	public boolean readyToFire();
	public boolean continueFiring();
	public void notifyNodeBecameInactive();
	
}
