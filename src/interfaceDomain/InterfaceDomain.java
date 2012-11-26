package interfaceDomain;

import java.io.FileNotFoundException;

import javax.xml.stream.XMLStreamException;

import application.Domain;
import application.DomainContainer;
import configurationDomain.ConfigurationDomain;
import configurationDomain.exceptions.FileAlreadyLoadedException;
import configurationDomain.exceptions.SectionAlreadyExistsException;
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
		try {
			configurationDomain.loadConfig("config/interface.xml", "interface");
		} catch (FileNotFoundException e) {
			System.out.println("Interface settings file not found (config/interface.xml)");
			System.exit(1);
		} catch (XMLStreamException e) {
			System.out.println("Error found in interface settings file (config/interface.xml)");
			System.out.println(e.getMessage());
			System.exit(1);
		} catch (FileAlreadyLoadedException e) {
			// Do nothing, all good
		} catch (SectionAlreadyExistsException e) {
			System.out.println("This configuration section has already been taken");
			System.exit(1);
		}
		
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
