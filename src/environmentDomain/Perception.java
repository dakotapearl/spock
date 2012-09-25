package environmentDomain;

import java.util.Observable;
import networkDomain.NetworkSignal;
import networkDomain.NetworkTransmitter;

// Observable?
// Separate process in InputProcess to deal with observables
public abstract class Perception extends Observable implements NetworkTransmitter {
	
	public EnvironmentDomain environmentDomain;
	
	public Perception(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
		
		tools.errorChecking.Log.created(this.getClass());
	}
	
	protected void sendSignalToNetwork(NetworkSignal signal) {
		setChanged();
		notifyObservers(signal);
	}
	
	public abstract void start();
	
}
