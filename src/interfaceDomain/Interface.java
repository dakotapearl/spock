package interfaceDomain;

import java.util.Observer;

public abstract class Interface extends Thread implements Observer {
	
	public InterfaceDomain interfaceDomain;
	
	public Interface(InterfaceDomain interfaceDomain) {
		this.interfaceDomain = interfaceDomain;
	}
	
	public abstract void initialise();
	public abstract void run();
	
}
