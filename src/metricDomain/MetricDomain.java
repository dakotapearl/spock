package metricDomain;

import configurationDomain.ConfigurationDomain;
import application.Domain;
import application.DomainContainer;
import tools.errorChecking.Log;

public class MetricDomain extends Domain {
	
	public MetricDomain(DomainContainer container) {
		super(container);
	}

	public ConfigurationDomain configurationDomain;
	
	@Override
	public void initialiseIndependent() {
		configurationDomain = container.configurationDomain;
		
		Log.write("Goal domain initialised (independent)");	
	}

	@Override
	public void initialiseInterconnected() {
		Log.write("Goal domain initialised (interconnected)");
	}
	
}
