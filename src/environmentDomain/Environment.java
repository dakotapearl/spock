package environmentDomain;

import interfaceDomain.InterfaceObservable;

import java.util.HashMap;

import tools.errorChecking.Assert;

/**
 * @author Loren Chorley
 */
public abstract class Environment extends Thread {

	public EnvironmentDomain environmentDomain;
	public HashMap<String, InterfaceObservable> interfaceObservables;
	
	public Environment(EnvironmentDomain environmentDomain) {
		Assert.AssertTrue("EnvironmentDomain correctly passed to Environment", environmentDomain != null);
		
		this.environmentDomain = environmentDomain;
		interfaceObservables = new HashMap<String, InterfaceObservable>();
		
		// Initialise interface variables
		
		
	}
	
	public abstract void run();

}
