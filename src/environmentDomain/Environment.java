package environmentDomain;

import tools.errorChecking.Assert;

public abstract class Environment {

	public EnvironmentDomain environmentDomain;
	
	public Environment(EnvironmentDomain environmentDomain) {
		Assert.AssertTrue("EnvironmentDomain correctly passed to Environment", environmentDomain != null);
		
		this.environmentDomain = environmentDomain;
	}
	
	public abstract void start();

}
