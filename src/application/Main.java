package application;

import configurationDomain.ConfigurationDomain;
import metricDomain.MetricDomain;
import interfaceDomain.InterfaceDomain;
import tools.errorChecking.Log;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;
import networkDomain.*;

/**
 * @author Loren Chorley
 */
public class Main {
	
	public static void main(String[] args) {

		Log.LoggingEnabled = true;
		Log.CreationLogsEnabled = true;
		Log.MechanismDebugEnabled = false;
		Log.TimeStampEnabled = true;
		Log.ThreadCreationEnabled = true;
		
		Log.write("Application (1): instantiating domains");
		
		DomainContainer container = new DomainContainer();
		
		container.configurationDomain = new ConfigurationDomain(container);
		container.experimentDomain = new ExperimentDomain(container);
		container.environmentDomain = new EnvironmentDomain(container);
		container.networkDomain  = new NetworkDomain(container);
		container.dataDomain = new DataDomain(container);
		container.metricDomain = new MetricDomain(container);
		container.interfaceDomain = new InterfaceDomain(container);
		
		/*
		 * Note that the configuration domain is initialised before
		 * the others domains and that all initialise functions precede
		 * all start (or run) methods. 
		 * Note also that the order of start (or run) methods cannot be
		 * guaranteed, since they run concurrently.
		 */
		Log.write("Application (2): initialising domains");
		
		container.configurationDomain.initialise();
		container.environmentDomain.initialise();
		container.networkDomain.initialise();
		container.experimentDomain.initialise();
		container.dataDomain.initialise();
		container.metricDomain.initialise();
		container.interfaceDomain.initialise();
		
		Log.write("Application (3): starting interface");
		container.interfaceDomain.start();
		
	}
}
