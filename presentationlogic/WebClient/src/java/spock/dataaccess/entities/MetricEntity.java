package spock.dataaccess.entities;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.Metric;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name="Metrics")
public class MetricEntity implements Metric {
    private static final long serialVersionUID = 1L;
    @Id
    private String id;
    @NotNull
    protected String CodeURL;

    public MetricEntity() {
    }

    public MetricEntity(String id, String CodeURL) {
        this.id = id;
        this.CodeURL = CodeURL;
    }
    
    @Override
    public String getId() {
        return id;
    }

    @Override
    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String getCodeURL() {
        return CodeURL;
    }

    @Override
    public void setCodeURL(String CodeURL) {
        this.CodeURL = CodeURL;
    }


    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof MetricEntity)) {
            return false;
        }
        MetricEntity other = (MetricEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.Metric[ id=" + id + " ]";
    }
    
}
