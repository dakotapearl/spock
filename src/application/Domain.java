package application;


public abstract class Domain {
	
	protected DomainContainer container;
	
	public Domain(DomainContainer container) {
		this.container = container;
	}
	
	public abstract void initialise();
	
}
