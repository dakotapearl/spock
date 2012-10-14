package networkDomain;

/**
 * @author Loren Chorley
 */
public interface NetworkTargetable {
	
	public void acceptSignal(NetworkSignal signal, NetworkTransmitter sender);
	public int getID();
	
}
