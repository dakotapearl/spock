package environmentDomain;

import java.util.Observable;

import tools.errorChecking.Log;
import networkDomain.NetworkSignal;
import networkDomain.NetworkTransmitter;

/**
 * @author Loren Chorley
 */
public abstract class Perception extends Observable implements NetworkTransmitter {
	
	public EnvironmentDomain environmentDomain;
	
	public Perception(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
		
		tools.errorChecking.Log.created(this.getClass());
	}
	
	public void sendSignalToNetwork(NetworkSignal signal) {
		Log.writeForMechanisms("Perception sent signal with datum: " + signal.getData().getDatum().getValue().toString());
		
		setChanged();
		notifyObservers(signal);
	}
	
	public abstract void start();
	
}
