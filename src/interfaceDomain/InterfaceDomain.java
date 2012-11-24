package interfaceDomain;

import application.Domain;
import application.DomainContainer;
import configurationDomain.ConfigurationDomain;
import interfaceDomain.swing.SwingInterface;
import dataDomain.DataDomain;
import metricDomain.MetricDomain;
import networkDomain.NetworkDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class InterfaceDomain extends Domain {

	public InterfaceDomain(DomainContainer container) {
		super(container);
		environmentDomain = container.environmentDomain;
		experimentDomain = container.experimentDomain;
		networkDomain = container.networkDomain;
		dataDomain = container.dataDomain;
		metricDomain = container.metricDomain;
		configurationDomain = container.configurationDomain;
	}

	public EnvironmentDomain environmentDomain;
	public ExperimentDomain experimentDomain;
	public NetworkDomain networkDomain;
	public DataDomain dataDomain;
	public MetricDomain metricDomain;
	public ConfigurationDomain configurationDomain;
	
	Interface i;
	
	public void initialise() {
		// Applet perhaps, that can display relevant details as well as pick, control, start, restart and stop experiments
		// turn off and on various logging comments
		
		// Check system type or check arguments to see what kind of interface is desired
		i = new SwingInterface(this);
		i.initialise();
		
		Log.write("Interface domain initialised");
	}
	
	public void registerInterfaceObservable(InterfaceObservable io) {
		Assert.CriticalAssertTrue("Interface is initialised when attempting to register interface observables", i != null);
		io.addObserver(i);
	}
	
	public void start() {
		i.start(); // (starts it's own thread of execution)
		// End of main thread
	}
	
}
