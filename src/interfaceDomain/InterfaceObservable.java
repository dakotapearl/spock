package interfaceDomain;

import java.util.Observable;

public class InterfaceObservable extends Observable {
	
	private String id;
	
	public InterfaceObservable(String id) {
		this.id = id;
	}
	
	public String getID() {
		return id;
	}
	
	public synchronized void updateInterface(String str) {
		setChanged();
		notifyObservers((Object) str);
	}
	
}
