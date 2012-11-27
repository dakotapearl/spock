package interfaceDomain;

import java.io.FileNotFoundException;

import javax.xml.stream.XMLStreamException;

import application.Domain;
import application.DomainContainer;
import configurationDomain.ConfigurationDomain;
import configurationDomain.exceptions.FileAlreadyLoadedException;
import configurationDomain.exceptions.SectionAlreadyExistsException;
import interfaceDomain.android.AndroidInterface;
import interfaceDomain.swing.SwingInterface;
import interfaceDomain.web.WebInterface;
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
	
	Interface instance;
	
	public void initialise() {
		
		String interfaceType;
		
		// Applet perhaps, that can display relevant details as well as pick, control, start, restart and stop experiments
		// turn off and on various logging comments
		try {
			configurationDomain.loadConfig("config/interface.xml", "interface");
		} catch (FileNotFoundException e) {
			System.out.println("Interface settings file not found (config/interface.xml)");
			System.exit(1);
			
			// TODO create default inteface config and save as interface.xml
			
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
		
		interfaceType = configurationDomain.getSetting("interface", "type"); 
		
		if (interfaceType.equals("swing")) {
			instance = new SwingInterface(this);
		} else if (interfaceType.equals("web")) {
			instance = new WebInterface(this);
		} else if (interfaceType.equals("android")) {
			instance = new AndroidInterface(this);
		//} else if (interfaceType.equals("swing")) {
			//instance = new WebServiceInterface(this);
		} else {
			System.out.println("Unknown interface type");
		}
			
		instance.initialise();
		
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
