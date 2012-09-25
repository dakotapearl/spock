package environmentDomain;

public abstract class Environment {

	EnvironmentDomain environmentDomain;
	
	public Environment(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
	}
	
	public abstract void start();

}
