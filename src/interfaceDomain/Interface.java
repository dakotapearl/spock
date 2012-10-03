package interfaceDomain;

public abstract class Interface extends Thread {
	
	public InterfaceDomain interfaceDomain;
	
	public Interface(InterfaceDomain interfaceDomain) {
		this.interfaceDomain = interfaceDomain;
	}
	
	public abstract void initialise();
	public abstract void start();
	
}
