package interfaceDomain;

import java.io.FileNotFoundException;

import javax.xml.stream.XMLStreamException;

import application.Domain;
import application.DomainContainer;
import configurationDomain.ConfigurationDomain;
import configurationDomain.exceptions.FileAlreadyLoadedException;
import configurationDomain.exceptions.SectionAlreadyExistsException;
import configurationDomain.exceptions.SectionNotFoundException;
import configurationDomain.exceptions.SettingNotFoundException;
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
	}

	public EnvironmentDomain environmentDomain;
	public ExperimentDomain experimentDomain;
	public NetworkDomain networkDomain;
	public DataDomain dataDomain;
	public MetricDomain metricDomain;
	public ConfigurationDomain configurationDomain;
	
	public Interface instance;
	public String interfaceType;
	
	public void registerInterfaceObservable(InterfaceObservable io) {
		Assert.CriticalAssertTrue("Interface is initialised when attempting to register interface observables", instance != null);
		io.addObserver(instance);
	}
	
	public void start() {
		instance.start(); // (starts it's own thread of execution)
		// End of main thread
	}

	@Override
	public void initialiseIndependent() {
		
		interfaceType = null;
		
		environmentDomain = container.environmentDomain;
		experimentDomain = container.experimentDomain;
		networkDomain = container.networkDomain;
		dataDomain = container.dataDomain;
		metricDomain = container.metricDomain;
		configurationDomain = container.configurationDomain;
		
		Log.write("Interface domain initialised (independent)");
	}

	@Override
	public void initialiseInterconnected() {
		
		// Applet perhaps, that can display relevant details as well as pick, control, start, restart and stop experiments
		// turn off and on various logging comments
		configurationDomain.loadConfigWithStandardHandling("config/interface.xml", "interface");
		
		// Check system type or check arguments to see what kind of interface is desired
		
		try {
			interfaceType = configurationDomain.getSetting("interface", "type");
		} catch (SectionNotFoundException e) {
			System.out.println("Configuration section interface does not exist");
			System.exit(1);
		} catch (SettingNotFoundException e) {
			System.out.println("Setting type not found in interface.xml");
			System.exit(1);
		} 
		
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
		
		
		Log.write("Interface domain initialised (interconnected)");
	}
	
}
