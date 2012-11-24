package metricDomain;

import configurationDomain.ConfigurationDomain;
import application.Domain;
import application.DomainContainer;
import tools.errorChecking.Log;

public class MetricDomain extends Domain {
	
	public MetricDomain(DomainContainer container) {
		super(container);
		configurationDomain = container.configurationDomain;
	}

	public ConfigurationDomain configurationDomain;
	
	public void initialise() {
		Log.write("Goal domain initialised");
	}
	
}
