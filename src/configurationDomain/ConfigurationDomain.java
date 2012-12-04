package configurationDomain;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import tools.errorChecking.Log;

import configurationDomain.configs.NetworkBehaviourConfig;
import configurationDomain.domainLoaders.NetworkBehaviourLoader;
import configurationDomain.exceptions.FileAlreadyLoadedException;
import configurationDomain.exceptions.SectionAlreadyExistsException;
import configurationDomain.exceptions.SectionNotFoundException;
import configurationDomain.exceptions.SettingNotFoundException;

import application.Domain;
import application.DomainContainer;

public class ConfigurationDomain extends Domain {
	
	// General settings
	private HashSet<String> loadedFiles;
	private HashMap<String, ConfigSection> generalSettings;
	
	// Domain specific data loaders
	private HashMap<String, DomainConfigLoader> loaderMap;
	
	public Settings settings;
	
	private class ConfigSection {
		public HashMap<String, String> settings;
		@SuppressWarnings("unused")
		public String path;
		public ConfigSection() {
			settings = new HashMap<>();
		}
	}
		
	public ConfigurationDomain(DomainContainer container) {
		super(container);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void initialiseIndependent() {
		
		DomainConfigLoader loader;
		
		generalSettings = new HashMap<String, ConfigSection>();
		loadedFiles = new HashSet<>();
		
		loaderMap = new HashMap<String, DomainConfigLoader>();
		
		settings = new Settings();
		
		// Let the domain config loaders register themselves
		loader = new NetworkBehaviourLoader(settings.networkbehaviours);
		loaderMap.put(loader.getTag(), loader);
		
		Log.write("Configuration domain initialised (independent)");
	}
	
	@Override
	public void initialiseInterconnected() {
		Log.write("Configuration domain initialised (interconnected)");
	}
	
	public String getSetting(String section, String name) throws SectionNotFoundException, SettingNotFoundException {
		ConfigSection sectionMap;
		
		if (!generalSettings.containsKey(section))
			throw new SectionNotFoundException();
		
		sectionMap = generalSettings.get(section);
		
		if (!sectionMap.settings.containsKey(name))
			throw new SettingNotFoundException();
		
		return generalSettings.get(section).settings.get(name);
	}
	
	public ConfigSection getSection(String section) throws SectionNotFoundException {
		
		if (!generalSettings.containsKey(section))
			throw new SectionNotFoundException();
		
		return generalSettings.get(section);
	}
	
	public void setSetting(String section, String name, String value) throws SectionNotFoundException {
		//TODO
	}
	
	public void addSection(String section) throws SectionAlreadyExistsException {
		
		if (generalSettings.containsKey(section))
			throw new SectionAlreadyExistsException();
		
		generalSettings.put(section, new ConfigSection());
		
	}
	
	public void removeSection(String section) throws SectionNotFoundException {
		generalSettings.remove(section);
	}
	
	public void saveConfig(String section) {
		//TODO
	}
	
	public void loadConfigWithStandardHandling(String filename, String newSection) {
		try {
			loadConfig(filename, newSection);
		} catch (FileNotFoundException e) {
			System.out.println("The settings file for '" + newSection + " was not found (" + filename + ")");
			// TODO create empty config and save
			System.exit(1);
		} catch (XMLStreamException e) {
			System.out.println("An error was found in the settings file for '" + newSection + "' (" + filename + ")");
			System.exit(1);
		} catch (FileAlreadyLoadedException e) {
			// Do nothing, all good
		} catch (SectionAlreadyExistsException e) {
			System.out.println("This configuration section name has already been taken (" + newSection + ")");
			System.exit(1);
		}
	}
	
	public void loadConfig(String filename, String newSection) throws FileAlreadyLoadedException, SectionAlreadyExistsException, XMLStreamException, FileNotFoundException {
		
		filename = "/home/loren/spock/" + filename; // TODO figure out relative pathing
		
		if (loadedFiles.contains(filename))
			throw new FileAlreadyLoadedException();
		
		loadedFiles.add(filename);
		
		XMLInputFactory factory = XMLInputFactory.newInstance();
		XMLEventReader r = factory.createXMLEventReader(filename, new FileInputStream(filename));
		XMLEvent e;
		
		String section = newSection;
		String upSection = newSection;
		ArrayList<String> sectionList = new ArrayList<String>();
		DomainConfig newConfig = null;
		String configType = "";
		String configProperty = null;
		
		addSection(newSection);
		
		// Make sure the document has a <configuration> tag as the main tag
		if (r.hasNext()) {
			
			try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
			
			if (!(e.getEventType() == XMLEvent.START_DOCUMENT)) {
				System.out.println("No start document"); // TODO
				return;
			}
			
			try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
			
			if (!(e.getEventType() == XMLEvent.START_ELEMENT && e.toString().equals("<configuration>"))) {
				System.out.println("<configuration> not the main tag: " + e.toString()); // TODO
				return;
			}
			
		}
		
		while (r.hasNext()) {
			
		    try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
		    
		    switch (e.getEventType()) {
		    case XMLEvent.START_ELEMENT:
		    	
		    	//if (e.asStartElement().getName().getLocalPart().equals("behaviour")) {
		    	if (loaderMap.containsKey(e.asStartElement().getName().getLocalPart())) {		    		
		    		configType = e.asStartElement().getName().getLocalPart();
		    		newConfig = loaderMap.get(configType).newConfig(configType);
		    	} else if (newConfig != null) { // enter behaviour property
		    		configProperty = e.asStartElement().getName().getLocalPart();
		    	} else {
			    	sectionList.add(e.asStartElement().getName().getLocalPart());
			    	upSection = section;
			    	section = section + "." + e.asStartElement().getName().getLocalPart();
		    	}
			    
		    	break;
		    	
		    case XMLEvent.END_ELEMENT:
		    	
		    	if (e.asEndElement().getName().getLocalPart().equals(configType)) {
		    		if (newConfig.isValid() && newConfig.getID() != null) {
		    			loaderMap.get(e.asEndElement().getName().getLocalPart()).setConfig(newConfig);
		    		} else { // invalid behaviour configuration
		    			if (newConfig.getID() != null) {
		    				System.out.println("Invalid config discarded: " + newConfig.getID());
		    				//TODO make exception or some sort of exception handler
		    			} else {
		    				System.out.println("Invalid config discarded");
		    			}
		    		}
		    		newConfig = null;
	    			configType = "";
		    	} else if (newConfig != null) { // leave behaviour property
		    		configProperty = null; 
		    	} else {
			    	if (sectionList.size() > 0) {
			    		section = upSection;
			    		if (sectionList.size() > 1)
			    			upSection = upSection.substring(0, section.length() - sectionList.get(sectionList.size() - 1).length()); 
			    		sectionList.remove(sectionList.size() - 1);
			    	} // TODO Check that this is right
		    	}
		    	break;
		    	
		    case XMLEvent.CHARACTERS:
		    	
		    	// If not white space
		    	if (!(e.isCharacters() && e.asCharacters().isWhiteSpace())) {
		    		
		    		if (!generalSettings.containsKey(upSection)) {
		    			//generalSettings.put(upSection, new HashMap<String, String>());
		    			System.out.println("Should not happen!");
		    			System.exit(1);
		    		}
		    		
		    		if (newConfig != null) { // enter behaviour property
		    			loaderMap.get(configType).setProperty(newConfig, configProperty, e.asCharacters().getData());
			    	} else {
				    	generalSettings.get(upSection).settings.put(sectionList.get(sectionList.size() - 1), e.asCharacters().getData());
			    	}
				    
		    	}
		    	
		    	break;
		    	
		    }
		}

	}
		
	// Testing
	public static void main(String[] args) {
		ConfigurationDomain c = new ConfigurationDomain(new DomainContainer());
		
		c.initialiseIndependent();
		
		try {
			c.loadConfig("config/interface.xml", "interface");
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		try {
			ConfigSection cs = c.getSection("interface");
			for (String s : cs.settings.keySet()) {
				System.out.println(s+": "+cs.settings.get(s));
			}
		} catch (SectionNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		try {
			System.out.println(c.getSetting("interface", "type"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		try {
			c.loadConfig("config/network.xml", "network");
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FileAlreadyLoadedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SectionAlreadyExistsException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		for (String bc : c.settings.networkbehaviours.keySet()) {
			System.out.println(bc + ": " + ((NetworkBehaviourConfig) c.settings.networkbehaviours.get(bc)).path);
		}
		
		System.out.println("fc: " + ((NetworkBehaviourConfig) c.settings.networkbehaviours.get("test")).path + ((NetworkBehaviourConfig) c.settings.networkbehaviours.get("test")).fcPath);
		
		
		
	}

	
	
}
