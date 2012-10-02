package environmentDomain;

import tools.errorChecking.Assert;

/**
 * @author Loren Chorley
 */
public abstract class Environment extends Thread {

	public EnvironmentDomain environmentDomain;
	
	public Environment(EnvironmentDomain environmentDomain) {
		Assert.AssertTrue("EnvironmentDomain correctly passed to Environment", environmentDomain != null);
		
		this.environmentDomain = environmentDomain;
	}
	
	public abstract void start();

}
